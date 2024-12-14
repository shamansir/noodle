module Demo.Toolkit.Starter.Repr where

import Prelude

import Color as Color

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap) as NT
import Data.String (splitAt, drop) as String
import Data.Number (fromString) as Number
import Data.Array (length) as Array
import Data.Int (fromString, toNumber) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T

import Noodle.Fn.ToFn (class PossiblyToFn)

import Noodle.Ui.Cli.Palette.Item (crepr) as C
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Tagging.At (class At, at, ChannelLabel)
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)


import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
    ( exprCtor, exprApp, exprIdent, exprBool, exprChar, exprNumber, exprRecord, exprInt, exprArray, exprOp
    , binaryOp
    , typeCtor, typeApp, typeOp
    , declImport, declImportAs, importOp, importTypeOp
    )

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.ChRepr (class ToChRepr, class FromChRepr)
import Noodle.Repr.ChRepr (wrap, unwrap, fromEq, toEq) as CR
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.FamilyDef.Codegen
    ( class CodegenRepr, Options(..)
    , class ValueCodegen
    , mkExpression
    , groupPascalCase, familyPascalCase
    )
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))


data StateRepr
    = StateRepr


data ValueRepr
    = VNone
    | VAny ValueRepr
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


data Any = Any ValueRepr


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


instance CodegenRepr ValueRepr where
    reprModule = const "Demo.Toolkit.Starter.Repr"
    reprTypeName = const "ValueRepr"
    reprType = const $ unsafePartial $ typeCtor "ValueRepr"
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
                Just "Time"    -> mkExpression (HF.fallback :: Time)
                Just "Color"   -> mkExpression (HF.fallback :: Color)
                Just "Shape"   -> mkExpression (HF.fallback :: Shape)
                Just "SpreadN" -> mkExpression (HF.fallback :: Spread Number)
                Just "SpreadV" -> mkExpression (HF.fallback :: Spread (Number /\ Number))
                Just "SpreadC" -> mkExpression (HF.fallback :: Spread Color)
                Just "SpreadS" -> mkExpression (HF.fallback :: Spread Shape)
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
                            Nothing    -> mkExpression (HF.fallback :: Color)
                    "s/" ->
                        case shapeFromString after of
                            Just shape -> mkExpression shape
                            Nothing    -> mkExpression (HF.fallback :: Shape)
                    "t/" ->
                        case timeFromString after of
                            Just time -> mkExpression time
                            Nothing   -> mkExpression (HF.fallback :: Time)
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


options :: Options ValueRepr
options = Options $
    { reprAt : { module_ : "Demo.Toolkit.Starter.Repr", type_ : "ValueRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "StarterTk" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , prepr : (Proxy :: _ ValueRepr)
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , imports : unsafePartial $
        [ declImport "Data.Tuple.Nested" [ importOp "/\\", importTypeOp "/\\" ]
        , declImportAs "Demo.Toolkit.Starter.Repr" [] "PR"
        ]
    }


instance HasFallback ValueRepr where
    fallback = VNone


instance FromChRepr ValueRepr ValueRepr where fromChRepr = CR.fromEq
instance ToChRepr   ValueRepr ValueRepr where toChRepr   = CR.toEq


instance ToChRepr Any     ValueRepr where toChRepr = Just <<< CR.wrap <<< case _ of Any pr -> pr
instance ToChRepr Bang    ValueRepr where toChRepr = Just <<< CR.wrap <<< const VBang
instance ToChRepr Boolean ValueRepr where toChRepr = Just <<< CR.wrap <<< VBool
instance ToChRepr Char    ValueRepr where toChRepr = Just <<< CR.wrap <<< VChar
instance ToChRepr Number  ValueRepr where toChRepr = Just <<< CR.wrap <<< VNumber
instance ToChRepr Time    ValueRepr where toChRepr = Just <<< CR.wrap <<< VTime
instance ToChRepr Shape   ValueRepr where toChRepr = Just <<< CR.wrap <<< VShape
instance ToChRepr Color   ValueRepr where toChRepr = Just <<< CR.wrap <<< VColor
instance ToChRepr (Spread Number) ValueRepr where toChRepr = Just <<< CR.wrap <<< VSpreadNum
instance ToChRepr (Spread (Number /\ Number)) ValueRepr where toChRepr = Just <<< CR.wrap <<< VSpreadVec
instance ToChRepr (Spread Color) ValueRepr where toChRepr = Just <<< CR.wrap <<< VSpreadCol
instance ToChRepr (Spread Shape) ValueRepr where toChRepr = Just <<< CR.wrap <<< VSpreadShp


{-
instance Mark ValueRepr where
    mark = case _ of
        VNone -> C.crepr $ X11.burlywood
        VAny repr -> mark repr
        VBang -> C.crepr $ X11.aqua
        VBool bool ->
            if bool
                then C.crepr $ X11.steelblue2
                else C.crepr $ X11.steelblue
            -- T.fgc (C.crepr $ X11.blue) $ T.s $ if bool then "T" else "F"
        VChar _ -> C.crepr $ X11.aquamarine
        VNumber _ -> C.crepr $ X11.green
        VTime _ -> C.crepr $ X11.darkolivegreen4
        VColor clr -> toNativeColor clr
        VShape _ ->
            C.crepr $ X11.firebrick1
        VSpreadNum _ -> C.crepr $ X11.darkolivegreen3
        VSpreadVec _ -> C.crepr $ X11.darkolivegreen3
        VSpreadCol _ -> C.crepr $ X11.darkolivegreen3
        VSpreadShp _ -> C.crepr $ X11.darkolivegreen3
        where
            toNativeColor :: Color -> Color.Color
            toNativeColor (Color { r, g, b, a }) = Color.rgba r g b $ Int.toNumber a / 255.0
-}


-- x == ChannelLabel
instance At x ValueRepr where
    at _ = case _ of
        VNone -> T.fgc (C.crepr $ X11.burlywood) $ T.s "∅"
        VAny repr -> at (Proxy :: _ x) repr
        VBang -> T.fgc (C.crepr $ X11.aqua) $ T.s "⊚" -- ⌾ ⏺
        VBool bool ->
            if bool
                then T.fgc (C.crepr $ X11.steelblue2) $ T.s "T"
                else T.fgc (C.crepr $ X11.steelblue)  $ T.s "F"
            -- T.fgc (C.crepr $ X11.blue) $ T.s $ if bool then "T" else "F"
        VChar ch -> T.fgc (C.crepr $ X11.aquamarine) $ T.s $ show ch
        VNumber num -> T.fgc (C.crepr $ X11.green) $ T.s $ show num
        VTime t -> T.fgc (C.crepr $ X11.darkolivegreen4) $ T.s $ show t
        VColor clr -> T.fgc (toNativeColor clr) $ T.s "■"
        VShape shape ->
            T.fgc (C.crepr $ X11.firebrick1) $ T.s $ case shape of -- TODO: replace with Unicode shapes if possible
                Circle -> "⏺"
                Rect -> "■"
                Cross -> "⨯"
                Diamond -> "◇"
        VSpreadNum spread -> channelSpread spread
        VSpreadVec spread -> channelSpread spread
        VSpreadCol spread -> channelSpread spread
        VSpreadShp spread -> channelSpread spread
        where
            channelSpread :: forall a. Spread a -> T.Tag
            channelSpread (Spread arr) = T.fgc (C.crepr $ X11.darkolivegreen3) $ T.wraps "[" "]" $ T.s $ show $ Array.length arr
            toNativeColor :: Color -> Color.Color
            toNativeColor (Color { r, g, b, a }) = Color.rgba r g b $ Int.toNumber a / 255.0


-- instance At a ValueRepr where
--     at _ _ = T.nil


instance Show Time where
    show (Time { seconds }) = show seconds <> "s"