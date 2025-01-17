module Demo.Toolkit.Starter.Repr.ChRepr where

import Prelude

import Effect (Effect)

import Color as Color

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap) as NT
import Data.String (splitAt, drop) as String
import Data.Number (fromString) as Number
import Data.Array (length) as Array
import Data.Int (fromString, toNumber) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T

import Noodle.Ui.Cli.Palette.Item (colorOf) as C
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Tagging.At (class At, at, ChannelLabel)
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)

import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE
import Cli.Components.Editor.Numeric as VNumeric
import Cli.Components.Editor.Textual as VTextual

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
     (binaryOp, exprApp, exprArray, exprBool, exprChar, exprCtor, exprIdent, exprInt, exprNumber, exprParens, exprRecord, typeApp, typeCtor, typeOp)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel, class ToValueInChannel, toValueInChannel, fromValueInChannel)
import Noodle.Repr.ValueInChannel (accept, decline, toMaybe) as ViC
import Noodle.Repr.Tagged (class Tagged)
import Noodle.Repr.Tagged (Path) as Tag
import Noodle.Raw.Fn.Shape (Tag, tagAs)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, class ValueCodegen, mkExpression, pDefaultFor, pValueFor)
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
-- import StarterTk.Simple.Gennum as Simple.Gennum


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


pValue = Proxy :: _ ValueRepr


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


instance HasFallback ValueRepr where
    fallback = VNone


instance FromValueInChannel ValueRepr ValueRepr where fromValueInChannel = identity
instance ToValueInChannel   ValueRepr ValueRepr where toValueInChannel   = ViC.accept


instance FromValueInChannel Any     ValueRepr where fromValueInChannel = case _ of Any pr -> pr
instance FromValueInChannel Bang    ValueRepr where fromValueInChannel = const VBang
instance FromValueInChannel Boolean ValueRepr where fromValueInChannel = VBool
instance FromValueInChannel Char    ValueRepr where fromValueInChannel = VChar
instance FromValueInChannel Number  ValueRepr where fromValueInChannel = VNumber
instance FromValueInChannel Time    ValueRepr where fromValueInChannel = VTime
instance FromValueInChannel Shape   ValueRepr where fromValueInChannel = VShape
instance FromValueInChannel Color   ValueRepr where fromValueInChannel = VColor
instance FromValueInChannel (Spread Number) ValueRepr where fromValueInChannel = VSpreadNum
instance FromValueInChannel (Spread (Number /\ Number)) ValueRepr where fromValueInChannel = VSpreadVec
instance FromValueInChannel (Spread Color) ValueRepr where fromValueInChannel = VSpreadCol
instance FromValueInChannel (Spread Shape) ValueRepr where fromValueInChannel = VSpreadShp


instance ToValueInChannel ValueRepr Number
    where
        toValueInChannel = _tryAny
            (case _ of
                VNumber num -> ViC.accept num
                _ -> ViC.decline
            )


instance ToValueInChannel ValueRepr Boolean
    where
        toValueInChannel = _tryAny
            (case _ of
                VBool bool -> ViC.accept bool
                _ -> ViC.decline
            )


instance ToValueInChannel ValueRepr Time
    where
        toValueInChannel = _tryAny
            (case _ of
                VTime time -> ViC.accept time
                _ -> ViC.decline
            )


instance Tagged ValueRepr where
    tag :: Tag.Path -> ValueRepr -> Tag
    tag = const $ tagAs <<< case _ of
        VNone -> "None"
        VBang -> "Bang"
        VBool _ -> "Bool"
        VChar _ -> "Char"
        VNumber _ -> "Number"
        VTime _ -> "Time"
        VColor _ -> "Color"
        VShape _ -> "Shape"
        VSpreadNum _ -> "NumberSpread"
        VSpreadVec _ -> "VectorSpread"
        VSpreadCol _ -> "ColorSpread"
        VSpreadShp _ -> "ShapeSpread"
        VAny _ -> "Any"


_tryAny :: forall a. (ValueRepr -> ValueInChannel a) -> ValueRepr -> ValueInChannel a
_tryAny f = case _ of
    VAny vrepr -> f vrepr -- not going deeper -- FIXME: why we need Any Any-Way? ... uh, it is for `Log` node input, to log everything that goes inside...
    vrepr      -> f vrepr


{-
instance Mark ValueRepr where
    mark = case _ of
        VNone -> C.colorOf $ X11.burlywood
        VAny repr -> mark repr
        VBang -> C.colorOf $ X11.aqua
        VBool bool ->
            if bool
                then C.colorOf $ X11.steelblue2
                else C.colorOf $ X11.steelblue
            -- T.fgc (C.colorOf $ X11.blue) $ T.s $ if bool then "T" else "F"
        VChar _ -> C.colorOf $ X11.aquamarine
        VNumber _ -> C.colorOf $ X11.green
        VTime _ -> C.colorOf $ X11.darkolivegreen4
        VColor clr -> toNativeColor clr
        VShape _ ->
            C.colorOf $ X11.firebrick1
        VSpreadNum _ -> C.colorOf $ X11.darkolivegreen3
        VSpreadVec _ -> C.colorOf $ X11.darkolivegreen3
        VSpreadCol _ -> C.colorOf $ X11.darkolivegreen3
        VSpreadShp _ -> C.colorOf $ X11.darkolivegreen3
        where
            toNativeColor :: Color -> Color.Color
            toNativeColor (Color { r, g, b, a }) = Color.rgba r g b $ Int.toNumber a / 255.0
-}


-- x == ChannelLabel
instance At x ValueRepr where
    at _ = case _ of
        VNone -> T.fgc (C.colorOf $ X11.burlywood) $ T.s "∅"
        VAny repr -> at (Proxy :: _ x) repr
        VBang -> T.fgc (C.colorOf $ X11.aqua) $ T.s "⊚" -- ⌾ ⏺
        VBool bool ->
            if bool
                then T.fgc (C.colorOf $ X11.steelblue2) $ T.s "T"
                else T.fgc (C.colorOf $ X11.steelblue)  $ T.s "F"
            -- T.fgc (C.colorOf $ X11.blue) $ T.s $ if bool then "T" else "F"
        VChar ch -> T.fgc (C.colorOf $ X11.aquamarine) $ T.s $ show ch
        VNumber num -> T.fgc (C.colorOf $ X11.green) $ T.s $ show num
        VTime t -> T.fgc (C.colorOf $ X11.darkolivegreen4) $ T.s $ show t
        VColor clr -> T.fgc (toNativeColor clr) $ T.s "■"
        VShape shape ->
            T.fgc (C.colorOf $ X11.firebrick1) $ T.s $ case shape of -- TODO: replace with Unicode shapes if possible
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
            channelSpread (Spread arr) = T.fgc (C.colorOf $ X11.darkolivegreen3) $ T.wraps "[" "]" $ T.s $ show $ Array.length arr
            toNativeColor :: Color -> Color.Color
            toNativeColor (Color { r, g, b, a }) = Color.rgba r g b $ Int.toNumber a / 255.0


-- instance At a ValueRepr where
--     at _ _ = T.nil


instance Show Time where
    show (Time { seconds }) = show seconds <> "s"


instance CodegenRepr ValueRepr where
    reprModule = const "Demo.Toolkit.Starter.Repr"
    reprTypeName = const "ValueRepr"
    reprType = const $ unsafePartial $ typeCtor "ValueRepr"
    fTypeFor = const $ unsafePartial $ \_ -> typeCtor "ValueRepr"
    fDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
            Just "Any"     -> exprCtor "VR.VNone"
            Just "Bang"    -> exprCtor "VR.VBang"
            Just "Bool"    -> exprApp (exprCtor "VR.VBool") [ pDefaultFor pValue mbType ]
            Just "Char"    -> exprApp (exprCtor "VR.VChar") [ pDefaultFor pValue mbType ]
            Just "Number"  -> exprApp (exprCtor "VR.VNumber") [ pDefaultFor pValue mbType ]
            Just "Time"    -> exprApp (exprCtor "VR.VTime") [ pDefaultFor pValue mbType ]
            Just "Color"   -> exprApp (exprCtor "VR.VColor") [ pDefaultFor pValue mbType ]
            Just "Shape"   -> exprApp (exprCtor "VR.VShape") [ pDefaultFor pValue mbType ]
            Just "SpreadN" -> exprApp (exprCtor "VR.VSpreadNum") [ pDefaultFor pValue mbType ]
            Just "SpreadV" -> exprApp (exprCtor "VR.VSpreadVec") [ pDefaultFor pValue mbType ]
            Just "SpreadC" -> exprApp (exprCtor "VR.VSpreadCol") [ pDefaultFor pValue mbType ]
            Just "SpreadS" -> exprApp (exprCtor "VR.VSpreadShp") [ pDefaultFor pValue mbType ]
    fValueFor = const $ unsafePartial $ \mbType encV ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
            Just "Any"     -> exprCtor "VR.VNone"
            Just "Bang"    -> exprCtor "VR.VBang"
            Just "Bool"    -> exprApp (exprCtor "VR.VBool")   [ pValueFor pValue mbType encV ]
            Just "Char"    -> exprApp (exprCtor "VR.VChar")   [ pValueFor pValue mbType encV ]
            Just "Number"  -> exprApp (exprCtor "VR.VNumber") [ pValueFor pValue mbType encV ]
            Just "Time"    -> exprApp (exprCtor "VR.VTime")   [ pValueFor pValue mbType encV ]
            Just "Color"   -> exprApp (exprCtor "VR.VColor")  [ pValueFor pValue mbType encV ]
            Just "Shape"   -> exprApp (exprCtor "VR.VShape")  [ pValueFor pValue mbType encV ]
            Just "SpreadN" -> exprApp (exprCtor "VR.VSpreadNum") [ pValueFor pValue mbType encV ]
            Just "SpreadV" -> exprApp (exprCtor "VR.VSpreadVec") [ pValueFor pValue mbType encV ]
            Just "SpreadC" -> exprApp (exprCtor "VR.VSpreadCol") [ pValueFor pValue mbType encV ]
            Just "SpreadS" -> exprApp (exprCtor "VR.VSpreadShp") [ pValueFor pValue mbType encV ]
    pTypeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
            case typeStr of
                "Any"     -> typeCtor "VR.Any"
                "Bang"    -> typeCtor "VR.Bang"
                "Bool"    -> typeCtor "Boolean"
                "Char"    -> typeCtor "Char"
                "Number"  -> typeCtor "Number"
                "Time"    -> typeCtor "VR.Time"
                "Color"   -> typeCtor "VR.Color"
                "Shape"   -> typeCtor "VR.Shape"
                "SpreadN" -> typeApp (typeCtor "VR.Spread") [ typeCtor "Number" ]
                "SpreadV" -> typeApp (typeCtor "VR.Spread")
                                [ typeOp (typeCtor "Number")
                                    [ binaryOp "/\\" $ typeCtor "Number" ]
                                ]
                "SpreadC" -> typeApp (typeCtor "VR.Spread") [ typeCtor "VR.Color" ]
                "SpreadS" -> typeApp (typeCtor "VR.Spread") [ typeCtor "VR.Shape" ]
                _ -> typeCtor "Unit"
    pDefaultFor = const $ unsafePartial $ \mbType ->
            case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
                Just "Any"     -> exprApp (exprCtor "VR.Any") [ exprCtor "VR.VNone" ]
                Just "Bang"    -> exprCtor "VR.Bang"
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
                _ -> exprCtor "VR.VNone"
    pValueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
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
                        case Number.fromString after of
                            Just n -> if n >= 0.0 then exprNumber n else exprParens $ exprNumber n
                            Nothing -> exprNumber 0.0
                    "c/" ->
                        case Color.fromHexString after of
                            Just color -> exprCtor "VR.VNone"
                            Nothing    -> mkExpression (HF.fallback :: Color)
                    "s/" ->
                        case shapeFromString after of
                            Just shape -> mkExpression shape
                            Nothing    -> mkExpression (HF.fallback :: Shape)
                    "t/" ->
                        case timeFromString after of
                            Just time -> mkExpression time
                            Nothing   -> mkExpression (HF.fallback :: Time)
                    _ -> exprCtor "VR.VNone"


instance ValueCodegen Shape where
    mkExpression = unsafePartial $ case _ of
        Circle -> exprCtor "VR.Circle"
        Rect -> exprCtor "VR.Rect"
        Cross -> exprCtor "VR.Cross"
        Diamond -> exprCtor "VR.Diamond"


instance ValueCodegen Time where
    mkExpression = unsafePartial $ case _ of
        Time { seconds } ->
            exprApp (exprCtor "VR.Time")
                [ exprRecord
                    [ "seconds" /\ exprInt seconds ]
                ]


instance ValueCodegen Color where
    mkExpression = unsafePartial $ case _ of
        Color { r, g, b, a } ->
            exprApp (exprCtor "VR.Color")
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
            exprApp (exprCtor "VR.Spread")
                [ exprArray $ mkExpression <$> items
                ]


editorFor :: ValueInChannel ValueRepr -> Maybe (ValueEditor ValueRepr Unit Effect)
editorFor = ViC.toMaybe >>> case _ of
    Just (VNumber _) ->
        Just $ VE.imap (maybe VNone VNumber) extractNum VNumeric.editor
            where
                extractNum = case _ of -- reuse `ValueInChannel`?
                    VNumber num -> Just num
                    _ -> Nothing
    _ -> Nothing