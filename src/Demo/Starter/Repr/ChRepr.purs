module StarterTk.Repr.ChRepr where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)

import Color (Color) as Native
import Color (rgba, fromHexString, toHexString, toRGBA) as NativeColor

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap) as NT
import Data.String (splitAt, drop, uncons) as String
import Data.String.CodeUnits as CU
import Data.Number (fromString) as Number
import Data.Array (length) as Array
import Data.Int (fromString, toNumber, round) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Either (Either(..))
import Data.Newtype (class Newtype)

import Data.String.Regex (Regex, split, regex, replace, search) as RGX
import Data.String.Regex.Flags (global, noFlags) as RGX

import Noodle.Ui.Cli.Palette.Item (colorOf) as C
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Tagging.At (class At, at, ChannelLabel)
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)

import Blessed.Internal.NodeKey (nk) as Blessed
import Cli.Keys (ValueEditorKey) as Key
import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE
import Cli.Components.Editor.Textual as VTextual
import Cli.Components.Editor.Numeric as VNumeric
import Cli.Components.Editor.Int as VInt
import Cli.Components.Editor.Color as VColor

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
     (binaryOp, exprApp, exprArray, exprBool, exprChar, exprCtor, exprIdent, exprInt, exprNumber, exprParens, exprRecord, typeApp, typeCtor, typeOp)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel, class ToValueInChannel, toValueInChannel, fromValueInChannel)
import Noodle.Repr.ValueInChannel (accept, decline, toMaybe) as ViC
import Noodle.Repr.Tagged (ValuePath, class ValueTagged) as VT
import Noodle.Raw.Fn.Shape (ValueTag, tagAs)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, class ValueCodegen, class ParseableRepr, class ValueEncode, mkExpression, pDefaultFor, pValueFor)
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
-- import StarterTk.Simple.Gennum as Simple.Gennum


data ValueRepr
    = VNone
    | VAny ValueRepr
    | VBang
    | VBool Boolean
    | VChar Char
    | VNumber Number
    | VInt Int
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


newtype Time = Time { hours :: Int, minutes :: Int, seconds :: Int }


derive instance Newtype Time _


newtype Color = Color { r :: Int, g :: Int, b :: Int, a :: Int }


data Spread a = Spread (Array a)


data Shape
    = Circle
    | Cross
    | Rect
    | Diamond


instance HasFallback Shape where fallback = Circle
instance HasFallback Time  where fallback = Time { hours : 0, minutes : 0, seconds : 0 }
instance HasFallback Color where fallback = Color { r : 0, g : 0, b : 0, a : 255 }
instance HasFallback (Spread a) where fallback = Spread []


shapeFromString :: String -> Maybe Shape
shapeFromString = case _ of
    "circle" -> Just Circle
    "rect" -> Just Rect
    "cross" -> Just Cross
    "diamond" -> Just Diamond
    _ -> Nothing


shapeToString :: Shape -> String
shapeToString = case _ of
    Circle -> "circle"
    Rect -> "rect"
    Cross -> "cross"
    Diamond -> "diamond"


timeFromString :: String -> Maybe Time
timeFromString str =
    let eSepRegex = RGX.regex "[hms]" RGX.global
    in case eSepRegex of
        Left _ -> Nothing
        Right sepRegex ->
            case RGX.split sepRegex str of
                [ hh, mm, ss, _ ] ->
                    Just $ Time
                        { hours   : fromMaybe 0 $ Int.fromString hh
                        , minutes : fromMaybe 0 $ Int.fromString mm
                        , seconds : fromMaybe 0 $ Int.fromString ss
                        }
                [ mm, ss, _ ] ->
                    Just $ Time
                        { hours   : 0
                        , minutes : fromMaybe 0 $ Int.fromString mm
                        , seconds : fromMaybe 0 $ Int.fromString ss
                        }
                [ ss, _ ] ->
                    Just $ Time
                        { hours   : 0
                        , minutes : 0
                        , seconds : fromMaybe 0 $ Int.fromString ss
                        }
                _ -> Nothing


timeToString :: Time -> String
timeToString (Time { hours, minutes, seconds }) =
    if hours > 0 then
        show hours <> "h" <> show minutes <> "m" <> show seconds <> "s"
    else if minutes > 0 then
        show minutes <> "m" <> show seconds <> "s"
    else
        show seconds <> "s"


fromNativeColor :: Native.Color -> Color
fromNativeColor = NativeColor.toRGBA >>> (\{ r, g, b, a } -> Color { r, g, b, a : 255 })
toNativeColor :: Color -> Native.Color
toNativeColor (Color { r, g, b, a }) = NativeColor.rgba r g b $ Int.toNumber a / 255.0



instance HasFallback ValueRepr where
    fallback = VNone


instance FromValueInChannel ValueRepr ValueRepr where fromValueInChannel = identity
instance ToValueInChannel   ValueRepr ValueRepr where toValueInChannel   = ViC.accept


instance FromValueInChannel Any     ValueRepr where fromValueInChannel = case _ of Any pr -> pr
instance FromValueInChannel Bang    ValueRepr where fromValueInChannel = const VBang
instance FromValueInChannel Boolean ValueRepr where fromValueInChannel = VBool
instance FromValueInChannel Char    ValueRepr where fromValueInChannel = VChar
instance FromValueInChannel Number  ValueRepr where fromValueInChannel = VNumber
instance FromValueInChannel Int     ValueRepr where fromValueInChannel = VInt
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


instance ToValueInChannel ValueRepr Int
    where
        toValueInChannel = _tryAny
            (case _ of
                VInt int -> ViC.accept int
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


instance VT.ValueTagged ValueRepr where
    valueTag :: VT.ValuePath -> ValueRepr -> ValueTag
    valueTag = const $ tagAs <<< case _ of
        VNone -> "None"
        VBang -> "Bang"
        VBool _ -> "Bool"
        VChar _ -> "Char"
        VNumber _ -> "Number"
        VInt _ -> "Int"
        VTime _ -> "Time"
        VColor _ -> "Color"
        VShape _ -> "Shape"
        VSpreadNum _ -> "NumberSpread"
        VSpreadVec _ -> "VectorSpread"
        VSpreadCol _ -> "ColorSpread"
        VSpreadShp _ -> "ShapeSpread"
        VAny _ -> "Any"
    acceptByTag :: Proxy ValueRepr -> { current :: ValueTag, incoming :: ValueTag } -> Boolean
    acceptByTag _ { current, incoming } = current == incoming


_tryAny :: forall a. (ValueRepr -> ValueInChannel a) -> ValueRepr -> ValueInChannel a
_tryAny f = case _ of
    VAny vrepr -> f vrepr -- not going deeper -- FIXME: why we need Any Any-Way? ... uh, it is for `Log` node input, to log everything that goes inside...
    vrepr      -> f vrepr


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
        VInt int -> T.fgc (C.colorOf $ X11.darkgreen) $ T.s $ show int
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
            toNativeColor :: Color -> Native.Color
            toNativeColor (Color { r, g, b, a }) = NativeColor.rgba r g b $ Int.toNumber a / 255.0


-- instance At a ValueRepr where
--     at _ _ = T.nil


instance Show Time where
    show = timeToString


encodeValueImpl :: ValueRepr -> Maybe EncodedValue
encodeValueImpl = case _ of
    VNone ->        Nothing
    VAny val ->     encodeValueImpl val
    VBang ->        Nothing
    VBool v ->      Just $ EncodedValue $ "b/" <> if v then "true" else "false"
    VNumber n ->    Just $ EncodedValue $ "#/" <> show n
    VInt n ->       Just $ EncodedValue $ "i/" <> show n
    VChar c ->      Just $ EncodedValue $ "x/" <> CU.singleton c
    VColor clr ->   Just $ EncodedValue $ "c/" <> (clr # toNativeColor # NativeColor.toHexString # String.drop 1)
    VTime time ->   Just $ EncodedValue $ "t/" <> timeToString time
    VShape shape -> Just $ EncodedValue $ "s/" <> shapeToString shape
    VSpreadNum _ -> Nothing -- TODO
    VSpreadVec _ -> Nothing -- TODO
    VSpreadCol _ -> Nothing -- TODO
    VSpreadShp _ -> Nothing -- TODO


toDefaultImpl :: EncodedType -> ValueRepr
toDefaultImpl = NT.unwrap >>> case _ of
    "Any"     -> VAny VNone
    "Bang"    -> VBang
    "Bool"    -> VBool false
    "Char"    -> VChar '-'
    "Number"  -> VNumber 0.0
    "Int"     -> VInt 0
    "Time"    -> VTime (HF.fallback :: Time)
    "Color"   -> VColor (HF.fallback :: Color)
    "Shape"   -> VShape (HF.fallback :: Shape)
    "SpreadN" -> VSpreadNum (HF.fallback :: Spread Number)
    "SpreadV" -> VSpreadVec (HF.fallback :: Spread (Number /\ Number))
    "SpreadC" -> VSpreadCol (HF.fallback :: Spread Color)
    "SpreadS" -> VSpreadShp (HF.fallback :: Spread Shape)
    _ -> VNone


toReprImpl :: EncodedType -> EncodedValue -> Maybe ValueRepr
toReprImpl eType eValue =
    case NT.unwrap eType of
        "Any" ->
            case valuePrefix of
                "b/" -> VAny <$> toReprImpl (EncodedType "Bool") eValue
                "x/" -> VAny <$> toReprImpl (EncodedType "Char") eValue
                "#/" -> VAny <$> toReprImpl (EncodedType "Number") eValue
                "i/" -> VAny <$> toReprImpl (EncodedType "Int") eValue
                "t/" -> VAny <$> toReprImpl (EncodedType "Time") eValue
                "c/" -> VAny <$> toReprImpl (EncodedType "Color") eValue
                "s/" -> VAny <$> toReprImpl (EncodedType "Shape") eValue
                _ -> Nothing
        "Bang" -> Just VBang
        "Bool" ->
            if valuePrefix == "b/" then
                case valueStr of
                    "true" -> Just $ VBool true
                    _ -> Just $ VBool false
            else Nothing
        "Char" ->
            if valuePrefix == "x/" then
                VChar <$> _.head <$> CU.uncons valueStr
            else Nothing
        "Number" ->
            if valuePrefix == "#/" then
                VNumber <$> Number.fromString valueStr
            else Nothing
        "Int" ->
            if valuePrefix == "i/" then
                VInt <$> Int.fromString valueStr
            else Nothing
        "Time" ->
            if valuePrefix == "t/" then
                VTime <$> timeFromString valueStr
            else Nothing
        "Color" ->
            if valuePrefix == "c/" then
                VColor <$> fromNativeColor <$> NativeColor.fromHexString ("#" <> valueStr)
            else Nothing
        "Shape" ->
            if valuePrefix == "s/" then
                VShape <$> shapeFromString valueStr
            else Nothing
        "SpreadN" -> Nothing -- TODO
        "SpreadV" -> Nothing -- TODO
        "SpreadC" -> Nothing -- TODO
        "SpreadS" -> Nothing -- TODO
        _ -> Nothing
    where
        valuePrefix /\ valueStr =
            case String.splitAt 2 $ NT.unwrap eValue of
                { before, after } -> before /\ after


-- TODO: FIXME: Use toDefault & toRepr in the `Codegen` implementations below + `ValueCodegen ValueRepr` + `TypeCodegen ValueRepr`

instance ParseableRepr ValueRepr where
    toDefault = toDefaultImpl
    toRepr = toReprImpl


instance CodegenRepr ValueRepr where
    reprModule = const "StarterTk.Repr.ChRepr"
    reprTypeName = const "ValueRepr"
    reprType = const $ unsafePartial $ typeCtor "ValueRepr"
    fTypeFor = const $ unsafePartial $ \_ -> typeCtor "ValueRepr"
    fDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
            Just "Any"     -> exprCtor "VR.VNone"
            Just "Bang"    -> exprCtor "VR.VBang"
            Just "Bool"    -> exprApp (exprCtor "VR.VBool")   [ pDefaultFor pValue mbType ]
            Just "Char"    -> exprApp (exprCtor "VR.VChar")   [ pDefaultFor pValue mbType ]
            Just "Number"  -> exprApp (exprCtor "VR.VNumber") [ pDefaultFor pValue mbType ]
            Just "Int"     -> exprApp (exprCtor "VR.VInt")    [ pDefaultFor pValue mbType ]
            Just "Time"    -> exprApp (exprCtor "VR.VTime")   [ pDefaultFor pValue mbType ]
            Just "Color"   -> exprApp (exprCtor "VR.VColor")  [ pDefaultFor pValue mbType ]
            Just "Shape"   -> exprApp (exprCtor "VR.VShape")  [ pDefaultFor pValue mbType ]
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
            Just "Int"     -> exprApp (exprCtor "VR.VInt")    [ pDefaultFor pValue mbType ]
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
                "Int"     -> typeCtor "Int"
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
                Just "Int"     -> exprInt 0
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
                    "i/" ->
                        case Int.fromString after of
                            Just n -> if n >= 0 then exprInt n else exprParens $ exprInt n
                            Nothing -> exprInt 0
                    "c/" ->
                        case NativeColor.fromHexString after of
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
        Time { hours, minutes, seconds } ->
            exprApp (exprCtor "VR.Time")
                [ exprRecord
                    [ "hours"   /\ exprInt hours
                    , "minutes" /\ exprInt minutes
                    , "seconds" /\ exprInt seconds
                    ]
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


instance ValueEncode ValueRepr where
    encodeValue = encodeValueImpl


editorFor :: ValueInChannel ValueRepr -> Maybe (ValueEditor ValueRepr Unit Effect)
editorFor = ViC.toMaybe >>> case _ of
    Just (VNumber _) ->
        Just $ VE.imap (maybe VNone VNumber) extractNum VNumeric.editor
            where
                extractNum = case _ of -- reuse `ValueInChannel`?
                    VNumber num -> Just num
                    _ -> Nothing
    Just (VInt _) ->
        Just $ VE.imap (maybe VNone VInt) extractInt VInt.editor
            where
                extractInt = case _ of -- reuse `ValueInChannel`?
                    VInt int -> Just int
                    _ -> Nothing
    Just (VColor _) ->
        Just $ VE.imap (maybe VNone VColor <<< map fromNativeColor) extractColor VColor.editor
            where
                extractColor :: ValueRepr -> Maybe Native.Color
                extractColor = case _ of -- reuse `ValueInChannel`?
                    VColor color -> Just $ toNativeColor color
                    _ -> Nothing
    Just (VTime _) ->
        Just $ VE.imap (maybe VNone VTime) extractTime timeEditor
            where
                timeEditor :: forall state m. MonadThrow Error m => ValueEditor (Maybe Time) state m
                timeEditor =
                    VE.imap timeFromString (map timeToString >>> fromMaybe "-")
                        $ VTextual.fromKey (Blessed.nk :: Key.ValueEditorKey "time-value-editor")
                extractTime :: ValueRepr -> Maybe Time
                extractTime = case _ of -- reuse `ValueInChannel`?
                    VTime time -> Just time
                    _ -> Nothing
    _ -> Nothing