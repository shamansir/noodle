module Demo.Toolkit.Starter.Repr where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap) as NT
import Data.String (splitAt, drop) as String
import Data.Number (fromString) as Number
import Data.Int (fromString) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Color as Color
import Data.Text.Format as T
import Noodle.Ui.Cli.Palette.Mark (class Mark)
import Noodle.Ui.Cli.Tagging.At (class At)

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
    ( exprCtor, exprApp, exprIdent, exprBool, exprChar, exprNumber, exprRecord, exprInt, exprArray, exprOp
    , binaryOp
    , typeCtor, typeApp, typeOp
    , declImport, declImportAs, importOp, importTypeOp
    )

import Noodle.Repr
    ( class HasFallback, fallback
    , wrap, unwrap
    , class ToRepr, class FromRepr
    )
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.FamilyDef.Codegen
    ( class CodegenRepr, Options(..)
    , class ValueCodegen
    , mkExpression
    , groupPascalCase, familyPascalCase
    )
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))


data StarterRepr
    = VNone
    | VAny StarterRepr
    | VBang
    | VBool Boolean
    | VChar Char
    | VNumber Number
    | VTime Time
    | VColor Color
    | VShape Shape
    | VSpreadNum (Spread Number)
    | VSpreadVec (Spread (Number /\ Number))
    | VSpreadCol (Spread Color)
    | VSpreadShp (Spread Shape)


data Any = Any StarterRepr


data Bang = Bang


newtype Time = Time { seconds :: Int }


newtype Color = Color { r :: Int, g :: Int, b :: Int, a :: Int }


data Spread a = Spread (Array a)


data Shape
    = Circle
    | Cross
    | Rect
    | Diamond


instance HasFallback Shape where fallback = Circle
instance HasFallback Time  where fallback = Time { seconds : 0 }
instance HasFallback Color where fallback = Color { r : 0, g : 0, b : 0, a : 255 }
instance HasFallback (Spread a) where fallback = Spread []


instance CodegenRepr StarterRepr where
    reprModule = const "Demo.Toolkit.Starter.Repr"
    reprTypeName = const "StarterRepr"
    reprType = const $ unsafePartial $ typeCtor "StarterRepr"
    typeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
            case typeStr of
                "Any"     -> typeCtor "PR.Any"
                "Bang"    -> typeCtor "PR.Bang"
                "Bool"    -> typeCtor "Boolean"
                "Char"    -> typeCtor "Char"
                "Number"  -> typeCtor "Number"
                "Time"    -> typeCtor "PR.Time"
                "Color"   -> typeCtor "PR.Color"
                "Shape"   -> typeCtor "PR.Shape"
                "SpreadN" -> typeApp (typeCtor "PR.Spread") [ typeCtor "Number" ]
                "SpreadV" -> typeApp (typeCtor "PR.Spread")
                                [ typeOp (typeCtor "Number")
                                    [ binaryOp "/\\" $ typeCtor "Number" ]
                                ]
                "SpreadC" -> typeApp (typeCtor "PR.Spread") [ typeCtor "PR.Color" ]
                "SpreadS" -> typeApp (typeCtor "PR.Spread") [ typeCtor "PR.Shape" ]
                _ -> typeCtor "Unit"
    defaultFor = const $ unsafePartial $ \mbType ->
            case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
                Just "Any"     -> exprApp (exprCtor "PR.Any") [ exprCtor "PR.VNone" ]
                Just "Bang"    -> exprCtor "PR.Bang"
                Just "Bool"    -> exprIdent "false"
                Just "Char"    -> exprChar '-'
                Just "Number"  -> exprNumber 0.0
                Just "Time"    -> mkExpression (fallback :: Time)
                Just "Color"   -> mkExpression (fallback :: Color)
                Just "Shape"   -> mkExpression (fallback :: Shape)
                Just "SpreadN" -> mkExpression (fallback :: Spread Number)
                Just "SpreadV" -> mkExpression (fallback :: Spread (Number /\ Number))
                Just "SpreadC" -> mkExpression (fallback :: Spread Color)
                Just "SpreadS" -> mkExpression (fallback :: Spread Shape)
                _ -> exprCtor "PR.VNone"
    valueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
            -- case NT.unwrap <$> mbType of
            --     Just "Bang"    -> const $ mkExpression Bang
                -- case String 2 valueStr of
            let
                { before, after } = String.splitAt 2 valueStr
            in
                case before of
                    "b/" ->
                        case after of
                            "true"  -> exprBool true
                            "false" -> exprBool false
                    "#/" ->
                        exprNumber $ case Number.fromString after of
                            Just n -> n
                            Nothing -> 0.0
                    "c/" ->
                        case Color.fromHexString after of
                            Just color -> exprCtor "PR.VNone"
                            Nothing    -> mkExpression (fallback :: Color)
                    "s/" ->
                        case shapeFromString after of
                            Just shape -> mkExpression shape
                            Nothing    -> mkExpression (fallback :: Shape)
                    "t/" ->
                        case timeFromString after of
                            Just time -> mkExpression time
                            Nothing   -> mkExpression (fallback :: Time)
                    _ -> exprCtor "PR.VNone"


instance ValueCodegen Shape where
    mkExpression = unsafePartial $ case _ of
        Circle -> exprCtor "PR.Circle"
        Rect -> exprCtor "PR.Rect"
        Cross -> exprCtor "PR.Cross"
        Diamond -> exprCtor "PR.Diamond"


instance ValueCodegen Time where
    mkExpression = unsafePartial $ case _ of
        Time { seconds } ->
            exprApp (exprCtor "PR.Time")
                [ exprRecord
                    [ "seconds" /\ exprInt seconds ]
                ]


instance ValueCodegen Color where
    mkExpression = unsafePartial $ case _ of
        Color { r, g, b, a } ->
            exprApp (exprCtor "PR.Color")
                [ exprRecord
                    [ "r" /\ exprInt r
                    , "g" /\ exprInt g
                    , "b" /\ exprInt b
                    , "a" /\ exprInt a
                    ]
                ]


instance ValueCodegen a => ValueCodegen (Spread a) where
    mkExpression = unsafePartial $ case _ of
        Spread items ->
            exprApp (exprCtor "PR.Spread")
                [ exprArray $ mkExpression <$> items
                ]


shapeFromString :: String -> Maybe Shape
shapeFromString = case _ of
    "circle" -> Just Circle
    "rect" -> Just Rect
    "cross" -> Just Cross
    "diamond" -> Just Diamond
    _ -> Nothing


timeFromString :: String -> Maybe Time
timeFromString str = -- TODO: parse properly, i.e `2h20m15s` or `30m` or `5s` ...
    Just $ Time { seconds : fromMaybe 0 $ Int.fromString $ String.drop 1 str }


options :: Options StarterRepr
options = Options $
    { reprAt : { module_ : "Demo.Toolkit.Starter.Repr", type_ : "StarterRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "StarterTk" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , prepr : (Proxy :: _ StarterRepr)
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , imports : unsafePartial $
        [ declImport "Data.Tuple.Nested" [ importOp "/\\", importTypeOp "/\\" ]
        , declImportAs "Demo.Toolkit.Starter.Repr" [] "PR"
        ]
    }


instance HasFallback StarterRepr where
    fallback = VNone


instance FromRepr StarterRepr StarterRepr where fromRepr = unwrap >>> Just
instance ToRepr   StarterRepr StarterRepr where toRepr = Just <<< wrap


instance ToRepr Any     StarterRepr where toRepr = Just <<< wrap <<< case _ of Any pr -> pr
instance ToRepr Bang    StarterRepr where toRepr = Just <<< wrap <<< const VBang
instance ToRepr Boolean StarterRepr where toRepr = Just <<< wrap <<< VBool
instance ToRepr Char    StarterRepr where toRepr = Just <<< wrap <<< VChar
instance ToRepr Number  StarterRepr where toRepr = Just <<< wrap <<< VNumber
instance ToRepr Time    StarterRepr where toRepr = Just <<< wrap <<< VTime
instance ToRepr Shape   StarterRepr where toRepr = Just <<< wrap <<< VShape
instance ToRepr Color   StarterRepr where toRepr = Just <<< wrap <<< VColor
instance ToRepr (Spread Number) StarterRepr where toRepr = Just <<< wrap <<< VSpreadNum
instance ToRepr (Spread (Number /\ Number)) StarterRepr where toRepr = Just <<< wrap <<< VSpreadVec
instance ToRepr (Spread Color) StarterRepr where toRepr = Just <<< wrap <<< VSpreadCol
instance ToRepr (Spread Shape) StarterRepr where toRepr = Just <<< wrap <<< VSpreadShp


instance Mark StarterRepr where
    mark  _  = Color.rgb 255 255 255


instance At p StarterRepr where
    at _ _ = T.nil