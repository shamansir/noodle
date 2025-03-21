module HydraTk.Repr.Target where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith, toUpper) as String
import Data.Array (length) as Array
import Data.Array ((:))
import Data.Newtype (unwrap)

import Type.Proxy (Proxy(..))

import Noodle.Text.Code.Target (Target)
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.FromCode (class CanParse, class FromCode, fromCode, fromParser, SourceError)
import Noodle.Fn.Signature (Signature, SignatureS, class ToSignature, sigs, toSignature, class PossiblyToSignature, possiblyToSignature, i, o)
import Noodle.Fn.Signature (Argument, Output, argName, argValue, empty) as Sig

import HydraTk.Types as HT
import HydraTk.Repr.Parser as RP
import HydraTk.Repr.Markers as PM


foreign import data HYDRA_V :: Target
-- Hydra value definition



hydraV :: _ HYDRA_V
hydraV = Proxy


instance ToCode HYDRA_V opts Int where toCode _ = const show
instance ToCode HYDRA_V opts Number where toCode _ = const show
instance ToCode HYDRA_V opts String where toCode _ = const identity


_encode :: forall a. ToCode HYDRA_V Unit a => a -> String
_encode = toCode hydraV unit


_decode :: forall a. FromCode HYDRA_V Unit a => String -> Either SourceError a
_decode = fromCode hydraV unit


instance ToCode HYDRA_V opts HT.Value where
    toCode :: Proxy HYDRA_V -> opts -> HT.Value -> String
    toCode _ _ = case _ of
        HT.None -> "0 V"
        HT.Undefined -> "U V"
        HT.Number n -> "N " <> _encode n
        HT.VArray vals ease -> "VA " <> _encode vals <> " $$ " <> _encode ease <> ""
        HT.Dep fn -> "D " <> _encode fn
        HT.Time -> "T V"
        HT.MouseX -> "MX V"
        HT.MouseY -> "MY V"
        HT.Width -> "W V"
        HT.Height -> "H V"
        HT.Pi -> "PI V"
        HT.Fft bin -> "A " <> _encode bin


instance ToCode HYDRA_V opts HT.Texture where
    toCode :: Proxy HYDRA_V -> opts -> HT.Texture -> String
    toCode _ _ = case _ of
        HT.Empty -> "EMP T"
        HT.Start src -> "S " <> _encode src
        HT.BlendOf { what, with } blend -> "B " <> _encode what <> PM._texSep <> _encode with <> PM._texSep <> _encode blend <> PM._texsEnd
        HT.Filter texture op -> "F " <> _encode texture <> PM._texSep <> _encode op <> PM._texsEnd
        HT.ModulateWith { what, with } mod -> "M " <> _encode what <> PM._texSep <> _encode with <> PM._texSep <> _encode mod <> PM._texsEnd
        HT.Geometry texture gmt -> "G " <> _encode texture <> PM._texSep <> _encode gmt <> PM._texsEnd
        HT.CallGlslFn { over, mbWith } fn ->
            "CALL " <> _encode over <> PM._texSep
                <> (
                    case mbWith of
                        Just with -> _encode with <> PM._texSep
                        Nothing -> mempty
                )
                <> _encode fn <> PM._texsEnd


instance ToCode HYDRA_V opts HT.Blend where
    toCode :: Proxy HYDRA_V -> opts -> HT.Blend -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts HT.ColorOp where
    toCode :: Proxy HYDRA_V -> opts -> HT.ColorOp -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts HT.Modulate where
    toCode :: Proxy HYDRA_V -> opts -> HT.Modulate -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts HT.Geometry where
    toCode :: Proxy HYDRA_V -> opts -> HT.Geometry -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts HT.TODO where
    toCode :: Proxy HYDRA_V -> opts -> HT.TODO -> String
    toCode _ _ = const "TODO"


instance ToCode HYDRA_V opts HT.Context where
    toCode :: Proxy HYDRA_V -> opts -> HT.Context -> String
    toCode _ _ (HT.Context { time }) = "{ " <> _encode time <> " }"


instance ToCode HYDRA_V opts HT.UpdateFn where
    toCode :: Proxy HYDRA_V -> opts -> HT.UpdateFn -> String
    toCode _ _ = const "UF" -- TODO


instance ToCode HYDRA_V opts HT.ExtSource where
    toCode :: Proxy HYDRA_V -> opts -> HT.ExtSource -> String
    toCode _ _ = case _ of
        HT.Camera n -> "C " <> show n
        HT.Sketch name -> "SK " <> name
        HT.Video -> "V X"
        HT.Unclear -> "U X"


instance ToCode HYDRA_V opts HT.Source where
    toCode :: Proxy HYDRA_V -> opts -> HT.Source -> String
    toCode _ _ = case _ of
        HT.Load outputN -> "O " <> _encode outputN
        HT.External sourceN def -> "X " <> _encode sourceN <> PM._argSep <> _encode def <> PM._argsEnd
        HT.From (HT.Gradient { speed }) -> "G " <> _encode speed <> PM._argsEnd
        HT.From (HT.Noise { scale, offset }) -> "N " <> _encode scale <> PM._argSep <> _encode offset <> PM._argsEnd
        HT.From (HT.Osc { frequency, sync, offset }) -> "OSC " <> _encode frequency <> PM._argSep <> _encode sync <> PM._argSep <> _encode offset <> PM._argsEnd
        HT.From (HT.Shape { sides, radius, smoothing }) -> "SHP " <> _encode sides <> PM._argSep <> _encode radius <> PM._argSep <> _encode smoothing <> PM._argsEnd
        HT.From (HT.Solid { r, g, b, a }) -> "S " <> _encode r <> PM._argSep <> _encode g <> PM._argSep <> _encode b <> PM._argSep <> _encode a <> PM._argsEnd
        HT.From (HT.Voronoi { scale, speed, blending }) -> "V " <> _encode scale <> PM._argSep <> _encode speed <> PM._argSep <> _encode blending <> PM._argsEnd


instance ToCode HYDRA_V opts HT.RenderTarget where
    toCode :: Proxy HYDRA_V -> opts -> HT.RenderTarget -> String
    toCode _ _ HT.Four = "ALL 4"
    toCode _ _ (HT.Output on) = "O" <> _encode on


instance ToCode HYDRA_V opts HT.Url where
    toCode :: Proxy HYDRA_V -> opts -> HT.Url -> String
    toCode _ _ (HT.Url url) = _encode url


instance ToCode HYDRA_V opts HT.GlslFnKind where
    toCode :: Proxy HYDRA_V -> opts -> HT.GlslFnKind -> String
    toCode _ _ = case _ of
        HT.FnSrc -> "SRC"
        HT.FnCoord -> "CRD"
        HT.FnCombineCoord -> "CCR"
        HT.FnCombine -> "CMB"
        HT.FnColor -> "CLR"


instance ToCode HYDRA_V opts HT.TOrV where
    toCode :: Proxy HYDRA_V -> opts -> HT.TOrV -> String
    toCode _ _ = case _ of
        HT.T tex -> "TT " <> _encode tex
        HT.V val -> "VV " <> _encode val


instance ToCode HYDRA_V opts HT.GlslFnCode where
    toCode :: Proxy HYDRA_V -> opts -> HT.GlslFnCode -> String
    toCode _ _ = case _ of
        HT.GlslFnCode str ->
            PM._glslStart <> _encode str <> PM._glslEnd


instance ToCode HYDRA_V opts HT.GlslFn where
    toCode :: Proxy HYDRA_V -> opts -> HT.GlslFn -> String
    toCode _ _ (HT.GlslFn { kind, code, fn })
        = _encode kind <> " "
            <> _encode code
            <> " " <> _encodeFnWithArgNames fn


instance ToCode HYDRA_V opts HT.GlslFnRef where
    toCode :: Proxy HYDRA_V -> opts -> HT.GlslFnRef -> String
    toCode _ _ (HT.GlslFnRef fn)
        = _encodeFnWithArgNames fn


{- instance ToCode HYDRA_V opts (Lang.Fn TOrV) where
    toCode :: Proxy HYDRA_V -> opts -> Lang.Fn TOrV -> String
    _encode _ = "" -}


instance ToCode HYDRA_V opts HT.CanBeSource where
    toCode :: Proxy HYDRA_V -> opts -> HT.CanBeSource -> String
    toCode _ _ (HT.CanBeSource cbs) =
        case cbs of
            Left sourceN -> "L " <> _encode sourceN
            Right outputN -> "R " <> _encode outputN


instance ToCode HYDRA_V opts HT.SourceOptions where
    toCode :: Proxy HYDRA_V -> opts -> HT.SourceOptions -> String
    toCode _ _ (HT.SourceOptions { src }) = "SO " -- TODO: <> _encode src


instance ToCode HYDRA_V opts HT.Values where
    toCode :: Proxy HYDRA_V -> opts -> HT.Values -> String
    toCode _ _ (HT.Values array) =
        if (Array.length array > 0) then
            PM._arrStart <> String.joinWith PM._arrSep (_encode <$> array) <> PM._arrEnd
        else
            PM._arrEmpty


instance ToCode HYDRA_V opts HT.Ease where
    toCode :: Proxy HYDRA_V -> opts -> HT.Ease -> String
    toCode _ _ = case _ of
        HT.Linear -> "LIN E"
        HT.Fast v -> "FST " <> _encode v
        HT.Smooth v -> "SMT " <> _encode v
        HT.Fit { low, high } -> "FIT " <> _encode low <> " < " <> _encode high
        HT.Offset v -> "OFF " <> _encode v
        HT.InOutCubic -> "IOC E"


instance ToCode HYDRA_V opts HT.AudioSource where
    toCode :: Proxy HYDRA_V -> opts -> HT.AudioSource -> String
    toCode _ _ = case _ of
        HT.Silence -> "SIL"
        HT.Mic -> "MIC"
        HT.File -> "FIL"


instance ToCode HYDRA_V opts HT.AudioBin where
    toCode :: Proxy HYDRA_V -> opts -> HT.AudioBin -> String
    toCode _ _ (HT.AudioBin n) = "@" <> show n


instance ToCode HYDRA_V opts HT.OutputN where
    toCode :: Proxy HYDRA_V -> opts -> HT.OutputN -> String
    toCode _ _ = case _ of
        HT.Output0 -> "O0"
        HT.Output1 -> "O1"
        HT.Output2 -> "O2"
        HT.Output3 -> "O3"
        HT.Output4 -> "O4"


instance ToCode HYDRA_V opts HT.SourceN where
    toCode :: Proxy HYDRA_V -> opts -> HT.SourceN -> String
    toCode _ _ = case _ of
        HT.Source0 -> "S0"


instance ToCode HYDRA_V opts HT.JsExpr where
    toCode :: Proxy HYDRA_V -> opts -> HT.JsExpr -> String
    toCode _ opts = case _ of
        HT.Val value -> encode value
        HT.AddE v1 v2 -> encode v1 <> " + " <> encode v2
        HT.SubE v1 v2 -> encode v1 <> " - " <> encode v2
        HT.MulE v1 v2 -> encode v1 <> " * " <> encode v2
        HT.DivE v1 v2 -> encode v1 <> " / " <> encode v2
        HT.ModE v1 v2 -> encode v1 <> " % " <> encode v2
        HT.Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> encode expr <> ")"
                    Nothing -> ""
                )
        HT.Brackets expr -> "( " <> encode expr <> " )"
        where
            encode :: forall a. ToCode HYDRA_V opts a => a -> String
            encode = toCode hydraV opts


instance ToCode HYDRA_V opts HT.DepFn where
    toCode :: Proxy HYDRA_V -> opts -> HT.DepFn -> String
    toCode _ _ = case _ of
        HT.UserExpr jsexpr -> _encode jsexpr
        HT.DepFn _ -> "[[CODE]]"
        HT.Unparsed str -> PM._unparsedFnStart <> str <> PM._unparsedFnEnd -- "<<<< " <> str <> " >>>>"
        HT.NoAction -> "/----/"


_encodeUsingFn :: forall a. ToSignature HYDRA_V HT.Value Unit a => a -> String
_encodeUsingFn a =
    case unwrap $ toSignature hydraV a :: SignatureS HT.Value Unit of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                String.toUpper name <> " " <> String.joinWith PM._argSep (_encode <$> Sig.argValue <$> args) <> PM._argsEnd
            else
                String.toUpper name <> " " <> PM._argsEnd


_encodeFnWithArgNames :: forall arg out. ToCode HYDRA_V Unit arg => Signature arg out -> String
_encodeFnWithArgNames fn =
    case unwrap $ toSignature (Proxy :: _ Void) fn :: SignatureS arg out of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                name <> " " <> show (Array.length args) <> " " <> String.joinWith PM._argSep (_encodeArg <$> args) <> PM._argsEnd
            else
                name <> " " <> show (Array.length args) <> " " <> PM._argsEnd
    where
        _encodeArg arg =
            Sig.argName arg <> "::" <> _encode (Sig.argValue arg)
    -- Fn.name fn


{- instance HasParser WrapRepr where
    parser = wrap -}


instance CanParse HYDRA_V HT.Value where
    parser = const RP.value


instance CanParse HYDRA_V HT.Texture where
    parser = const RP.texture


instance CanParse HYDRA_V HT.Source where
    parser = const RP.source


instance CanParse HYDRA_V HT.OutputN where
    parser = const RP.outputN


instance CanParse HYDRA_V HT.AudioBin where
    parser = const RP.audioBin


instance CanParse HYDRA_V HT.TODO where
    parser = const RP.todo


instance CanParse HYDRA_V HT.Values where
    parser = const RP.values


instance CanParse HYDRA_V HT.GlslFn where
    parser = const RP.glsl


instance CanParse HYDRA_V HT.AudioSource where
    parser = const RP.audioSrc


instance CanParse HYDRA_V HT.RenderTarget where
    parser = const RP.renderTarget


instance CanParse HYDRA_V HT.SourceN where
    parser = const RP.sourceN


instance CanParse HYDRA_V HT.Ease where
    parser = const RP.ease


instance FromCode HYDRA_V opts HT.Value    where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Texture  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Source   where fromCode = fromParser
instance FromCode HYDRA_V opts HT.OutputN  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.AudioBin where fromCode = fromParser
instance FromCode HYDRA_V opts HT.TODO     where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Values   where fromCode = fromParser
instance FromCode HYDRA_V opts HT.GlslFn   where fromCode = fromParser
instance FromCode HYDRA_V opts HT.AudioSource  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.RenderTarget where fromCode = fromParser
instance FromCode HYDRA_V opts HT.SourceN  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Ease     where fromCode = fromParser


-- TODO: generate it automatically from NDF
instance ToSignature HYDRA_V HT.Value Unit HT.ColorOp where
    toSignature :: Proxy _ -> HT.ColorOp -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.R { scale, offset } -> "r" /\ [ i "scale" scale, i "offset" offset ] /\ [ o "out" unit ]
        HT.G { scale, offset } -> "g" /\ [ i "scale" scale, i "offset" offset ] /\ [ o "out" unit ]
        HT.B { scale, offset } -> "b" /\ [ i "scale" scale, i "offset" offset ] /\ [ o "out" unit ]
        HT.A { scale, offset } -> "a" /\ [ i "scale" scale, i "offset" offset ] /\ [ o "out" unit ]
        HT.Posterize { bins, gamma } -> "posterize" /\ [ i "bins" bins, i "gamma" gamma ] /\ [ o "out" unit ]
        HT.Shift { r, g, b, a } -> "shift" /\ [ i "r" r, i "g" g, i "b" b, i "a" a ] /\ [ o "out" unit ]
        HT.Invert amount -> "invert" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Contrast amount -> "contrast" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Brightness amount -> "brightness" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Luma { threshold, tolerance } -> "luma" /\ [ i "threshold" threshold, i "tolerance" tolerance ] /\ [ o "out" unit ]
        HT.Thresh { threshold, tolerance } -> "thresh" /\ [ i "threshold" threshold, i "tolerance" tolerance ] /\ [ o "out" unit ]
        HT.Color { r, g, b, a } -> "color" /\ [ i "r" r, i "g" g, i "b" b, i "a" a ] /\ [ o "out" unit ]
        HT.Saturate amount -> "saturate" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Hue amount -> "hue" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Colorama amount -> "colorama" /\ [ i "amount" amount ] /\ [ o "out" unit ]


-- TODO: generate it automatically from NDF
instance ToSignature HYDRA_V HT.Value Unit HT.Modulate where
    toSignature :: Proxy _ -> HT.Modulate -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.Modulate amount -> "modulate" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.ModHue amount -> "modHue" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.ModKaleid { nSides } -> "modKaleid" /\ [ i "nSides" nSides ] /\ [ o "out" unit ]
        HT.ModPixelate { multiple, offset } -> "modPixelate" /\ [ i "multiple" multiple, i "offset" offset ] /\ [ o "out" unit ]
        HT.ModRepeat { repeatX, repeatY, offsetX, offsetY } -> "modRepeat" /\ [ i "repeatX" repeatX, i "repeatY" repeatY, i "offsetX" offsetX, i "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.ModRepeatX { reps, offset } -> "modRepeatX" /\ [ i "reps" reps, i "offset" offset ] /\ [ o "out" unit ]
        HT.ModRepeatY { reps, offset } -> "modRepeatY" /\ [ i "reps" reps, i "offset" offset ] /\ [ o "out" unit ]
        HT.ModRotate { multiple, offset } -> "modRotate" /\ [ i "multiple" multiple, i "offset" offset ] /\ [ o "out" unit ]
        HT.ModScale { multiple, offset } -> "modScale" /\ [ i "multiple" multiple, i "offset" offset ] /\ [ o "out" unit ]
        HT.ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ i "scrollX" scrollX, i "scrollY" scrollY, i "speedX" speedX, i "speedY" speedY ] /\ [ o "out" unit ]
        HT.ModScrollX { scrollX, speed } -> "modScrollX" /\ [ i "scrollX" scrollX, i "speed" speed ] /\ [ o "out" unit ]
        HT.ModScrollY { scrollY, speed } -> "modScrollY" /\ [ i "scrollY" scrollY, i "speed" speed ] /\ [ o "out" unit ]


instance ToSignature HYDRA_V HT.Value Unit HT.Blend where
    toSignature :: Proxy _ -> HT.Blend -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.Blend amount -> "blend" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Add amount -> "add" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Sub amount -> "sub" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Mult amount -> "mult" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Layer amount -> "layer" /\ [ i "amount" amount ] /\ [ o "out" unit ]
        HT.Diff -> "diff" /\ [] /\ []
        HT.Mask -> "mask" /\ [] /\ []


instance ToSignature HYDRA_V HT.Value Unit HT.Geometry where
    toSignature :: Proxy _ -> HT.Geometry -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.GKaleid { nSides } -> "kaleid" /\ [ i "nSides" nSides ] /\ [ o "out" unit ]
        HT.GPixelate { pixelX, pixelY } -> "pixelate" /\ [ i "pixelX" pixelX, i "pixelY" pixelY ] /\ [ o "out" unit ]
        HT.GRepeat { repeatX, repeatY, offsetX, offsetY } -> "repeat" /\ [ i "repeatX" repeatX, i "repeatY" repeatY, i "offsetX" offsetX, i "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.GRepeatX { reps, offset } -> "repeatX" /\ [ i "reps" reps, i "offset" offset ] /\ [ o "out" unit ]
        HT.GRepeatY { reps, offset } -> "repeatY" /\ [ i "reps" reps, i "offset" offset ] /\ [ o "out" unit ]
        HT.GRotate { angle, speed } -> "rotate" /\ [ i "angle" angle, i "speed" speed ] /\ [ o "out" unit ]
        HT.GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ i "amount" amount, i "xMult" xMult, i "yMult" yMult, i "offsetX" offsetX, i "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ i "scrollX" scrollX, i "scrollY" scrollY, i "speedX" speedX, i "speedY" speedY ] /\ [ o "out" unit ]
        HT.GScrollX { scrollX, speed } -> "scrollX" /\ [ i "scrollX" scrollX, i "speed" speed ] /\ [ o "out" unit ]
        HT.GScrollY { scrollY, speed } -> "scrollY" /\ [ i "scrollY" scrollY, i "speed" speed ] /\ [ o "out" unit ]


instance ToSignature HYDRA_V HT.Value Unit HT.Ease where
    toSignature :: Proxy _ -> HT.Ease -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.Linear -> "linear" /\ [] /\ [ o "out" unit ]
        HT.Fast v -> "fast" /\ [ i "v" v ] /\ [ o "out" unit ]
        HT.Smooth v -> "smooth" /\ [ i "v" v ] /\ [ o "out" unit ]
        HT.Fit { low, high } -> "fit" /\ [ i "low" low, i "high" high ] /\ [ o "out" unit ]
        HT.Offset v -> "offset" /\ [ i "v" v ] /\ [ o "out" unit ]
        HT.InOutCubic -> "inOutCubic" /\ [] /\ [ o "out" unit ]


instance ToSignature HYDRA_V HT.GlslFnArg HT.GlslFnOut HT.GlslFn where
    toSignature :: Proxy _ -> HT.GlslFn -> Signature HT.GlslFnArg HT.GlslFnOut
    toSignature ph (HT.GlslFn { fn }) = toSignature (Proxy :: _ Void) fn


instance ToSignature HYDRA_V HT.GlslFnArg HT.GlslFnOut HT.GlslFnRef where
    toSignature :: Proxy _ -> HT.GlslFnRef -> Signature HT.GlslFnArg HT.GlslFnOut
    toSignature ph (HT.GlslFnRef fn) = toSignature (Proxy :: _ Void) fn


instance ToSignature HYDRA_V HT.Value Unit HT.From where
    toSignature :: Proxy _ -> HT.From -> Signature HT.Value Unit
    toSignature = const $ sigs <<< case _ of
        HT.Gradient { speed } -> "gradient" /\ [ i "speed" speed ] /\ [ o "out" unit ]
        HT.Noise { scale, offset } -> "noise" /\ [ i "scale" scale, i "offset" offset ] /\ [ o "out" unit ]
        HT.Osc { frequency, sync, offset } -> "osc" /\ [ i "frequency" frequency, i "sync" sync, i "offset" offset ] /\ [ o "out" unit ]
        HT.Shape { sides, radius, smoothing } -> "shape" /\ [ i "sides" sides, i "radius" radius, i "smoothing" smoothing ] /\ [ o "out" unit ]
        HT.Solid { r, g, b, a } -> "solid" /\ [ i "r" r, i "g" g, i "b" b, i "a" a ] /\ [ o "out" unit ]
        HT.Voronoi { scale, speed, blending } -> "voronoi" /\ [ i "scale" scale, i "speed" speed, i "blending" blending ] /\ [ o "out" unit ]


instance PossiblyToSignature HYDRA_V HT.Value Unit HT.Source where
    possiblyToSignature :: Proxy _ -> HT.Source -> Maybe (Signature HT.Value Unit)
    possiblyToSignature ph = case _ of
        HT.Load outputN -> Nothing -- TODO: could be converted to `src()`
        HT.External sourceN ext -> Nothing -- TODO: could be converted to `src()` ?
        HT.From from -> Just $ toSignature ph from


instance PossiblyToSignature HYDRA_V HT.TOrV HT.OTOrV HT.Texture where
    possiblyToSignature :: Proxy _ -> HT.Texture -> Maybe (Signature HT.TOrV HT.OTOrV)
    possiblyToSignature ph = case _ of
        HT.Empty -> Nothing
        HT.Start src ->
            case (unwrap <$> possiblyToSignature ph src :: Maybe (SignatureS HT.Value Unit)) of
                Just (name /\ args /\ outs) -> Just $ sigs $ name /\ (map HT.V <$> args) /\ (map (const HT.OT) <$> outs)
                Nothing -> Nothing
        HT.BlendOf { what, with } blend ->
            case unwrap $ toSignature ph blend :: SignatureS HT.Value Unit of
                name /\ args /\ outs -> Just $ sigs $ name /\ ((i "what" $ HT.T what) : (map HT.V <$> args) <> [ i "with" $ HT.T with ]) /\ (map (const HT.OT) <$> outs)
        HT.Filter texture cop ->
            case unwrap $ toSignature ph cop :: SignatureS HT.Value Unit of
                name /\ args /\ outs -> Just $ sigs $ name /\ ((map HT.V <$> args) <> [ i "texture" $ HT.T texture ]) /\ (map (const HT.OT) <$> outs)
        HT.ModulateWith { what, with } mod ->
            case unwrap $ toSignature ph mod :: SignatureS HT.Value Unit of
                name /\ args /\ outs -> Just $ sigs $ name /\ ((i "what" $ HT.T what) : (map HT.V <$> args) <> [ i "with" $ HT.T with ]) /\ (map (const HT.OT) <$> outs)
        HT.Geometry texture gmt ->
            case unwrap $ toSignature ph gmt :: SignatureS HT.Value Unit of
                name /\ args /\ outs -> Just $ sigs $ name /\ ((map HT.V <$> args) <> [ i "texture" $ HT.T texture ]) /\ (map (const HT.OT) <$> outs)
        HT.CallGlslFn { over, mbWith } fnRef ->
            case unwrap $ toSignature ph fnRef :: SignatureS HT.TOrV HT.GlslFnOut of
                name /\ args /\ outs -> Just $ sigs $ name /\ ((i "over" $ HT.T over) : args <>
                    case mbWith of
                        Just with -> [ i "with" $ HT.T with ]
                        Nothing -> [ ]
                    ) /\ (map (const HT.OT) <$> outs)


instance PossiblyToSignature HYDRA_V HT.Value Unit HT.HydraFnId where
    possiblyToSignature :: Proxy _ -> HT.HydraFnId -> Maybe (Signature HT.Value Unit)
    possiblyToSignature _ = unwrap >>> fromKnownFn


instance PossiblyToSignature HYDRA_V HT.FnArg HT.FnOut HT.HydraFnId where
    possiblyToSignature _ = unwrap >>> defaultsFor


-- TODO: generate from NDF
defaultsFor :: String -> Maybe (Signature HT.FnArg HT.FnOut) -- TODO: output of Fn!
-- defaultValuesOf :: FamilyR -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
defaultsFor = map sigs <<< case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ i "scale" $ HT.narg 10.0, i "offset" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "voronoi" -> Just $ "voronoi" /\ [ i "scale" $ HT.narg 5.0, i "speed" $ HT.narg 0.3, i "blending" $ HT.narg 0.3 ] /\ [ o "out" HT.tOut ]
    "osc" -> Just $ "osc" /\ [ i "frequency" $ HT.narg 60.0, i "sync" $ HT.narg 0.1, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "shape" -> Just $ "shape" /\ [ i "sides" $ HT.narg 3.0, i "radius" $ HT.narg 0.3, i "smoothing" $ HT.narg 0.01 ] /\ [ o "out" HT.tOut ]
    "gradient" -> Just $ "gradient" /\ [ i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "src" -> Just $ "src" /\ [ i "src" HT.sourceArg  ] /\ [ o "out" HT.tOut ]
    "solid" -> Just $ "solid" /\ [ i "r" $ HT.narg 0.0, i "g" $ HT.narg 0.0, i "b" $ HT.narg 0.0, i "a" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ i "what" HT.tArg, i "angle" $ HT.narg 10.0, i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scale" -> Just $ "scale" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 1.5, i "xMult" $ HT.narg 1.0, i "yMult" $ HT.narg 1.0, i "offsetX" $ HT.narg 0.5, i "offsetY" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "pixelate" -> Just $ "pixelate" /\ [ i "what" HT.tArg, i "pixelX" $ HT.narg 20.0, i "pixelY" $ HT.narg 20.0 ] /\ [ o "out" HT.tOut ]
    "repeat" -> Just $ "repeat" /\ [ i "what" HT.tArg, i "repeatX" $ HT.narg 3.0, i "repeatY" $ HT.narg 3.0, i "offsetX" $ HT.narg 0.0, i "offsetY" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "repeatX" -> Just $ "repeatX" /\ [ i "what" HT.tArg, i "reps" $ HT.narg 3.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "repeatY" -> Just $ "repeatY" /\ [ i "what" HT.tArg, i "reps" $ HT.narg 3.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "kaleid" -> Just $ "kaleid" /\ [ i "what" HT.tArg, i "nSides" $ HT.narg 4.0 ] /\ [ o "out" HT.tOut ]
    "scroll" -> Just $ "scroll" /\ [ i "what" HT.tArg, i "scrollX" $ HT.narg 0.5, i "scrollY" $ HT.narg 0.5, i "speedX" $ HT.narg 0.0, i "speedY" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scrollX" -> Just $ "scrollX" /\ [ i "what" HT.tArg, i "scrollX" $ HT.narg 0.5, i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scrollY" -> Just $ "scrollY" /\ [ i "what" HT.tArg, i "scrollY" $ HT.narg 0.5, i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]

    "posterize" -> Just $ "posterize" /\ [ i "what" HT.tArg, i "bins" $ HT.narg 3.0, i "gamma" $ HT.narg 6.0 ] /\ [ o "out" HT.tOut ]
    "shift" -> Just $ "shift" /\ [ i "what" HT.tArg, i "r" $ HT.narg 0.5, i "g" $ HT.narg 0.0, i "b" $ HT.narg 0.0, i "a" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "invert" -> Just $ "invert" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "contrast" -> Just $ "contrast" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 1.6 ] /\ [ o "out" HT.tOut ]
    "brightness" -> Just $ "brightness" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 0.4 ] /\ [ o "out" HT.tOut ]
    "luma" -> Just $ "luma" /\ [ i "what" HT.tArg, i "threshold" $ HT.narg 0.5, i "tolerance" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "thresh" -> Just $ "thresh" /\ [ i "what" HT.tArg, i "threshold" $ HT.narg 0.5, i "tolerance" $ HT.narg 0.04 ] /\ [ o "out" HT.tOut ]
    "color" -> Just $ "color" /\ [ i "what" HT.tArg, i "r" $ HT.narg 1.0, i "g" $ HT.narg 1.0, i "b" $ HT.narg 1.0, i "a" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "saturate" -> Just $ "saturate" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 2.0 ] /\ [ o "out" HT.tOut ]
    "hue" -> Just $ "hue" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 0.4 ] /\ [ o "out" HT.tOut ]
    "colorama" -> Just $ "colorama" /\ [ i "what" HT.tArg, i "amount" $ HT.narg 0.005 ] /\ [ o "out" HT.tOut ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ i "what" HT.tArg, i "scale" $ HT.narg 1.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "g" -> Just $ "g" /\ [ i "what" HT.tArg, i "scale" $ HT.narg 1.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "b" -> Just $ "b" /\ [ i "what" HT.tArg, i "scale" $ HT.narg 1.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "a" -> Just $ "a" /\ [ i "what" HT.tArg, i "scale" $ HT.narg 1.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "sub" -> Just $ "sub" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "layer" -> Just $ "layer" /\ [ i "what" HT.tArg, i "with" HT.tArg ] /\ [ o "out" HT.tOut ]
    "blend" -> Just $ "blend" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "mult" -> Just $ "mult" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "diff" -> Just $ "diff" /\ [ i "what" HT.tArg, i "with" HT.tArg ] /\ [ o "out" HT.tOut ]
    "mask" -> Just $ "mask" /\ [ i "what" HT.tArg, i "with" HT.tArg ] /\ [ o "out" HT.tOut ]

    "modulateRepeat" -> Just $ "modRepeat" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "repeatX" $ HT.narg 3.0, i "repeatY" $ HT.narg 3.0, i "offsetX" $ HT.narg 0.5, i "offsetY" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "reps" $ HT.narg 3.0, i "offset" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "reps" $ HT.narg 3.0, i "offset" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "nSides" $ HT.narg 4.0 ] /\ [ o "out" HT.tOut ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "scrollX" $ HT.narg 0.5, i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "scrollY" $ HT.narg 0.5, i "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulate" -> Just $ "modulate" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "modulateScale" -> Just $ "modScale" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "multiple" $ HT.narg 1.0, i "offset" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "multiple" $ HT.narg 10.0, i "offset" $ HT.narg 3.0 ] /\ [ o "out" HT.tOut ]
    "modulateRotate" -> Just $ "modRotate" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "multiple" $ HT.narg 1.0, i "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulateHue" -> Just $ "modHue" /\ [ i "what" HT.tArg, i "with" HT.tArg, i "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]

    "initCam" -> Just $ "initCam" /\ [] /\ []
    "initImage" -> Just $ "initImage" /\ [ i "url" HT.urlArg ] /\ []
    "initVideo" -> Just $ "initVideo" /\ [ i "url" HT.urlArg ] /\ []
    "init" -> Just $ "init" /\ [ i "options" HT.optionsArg ] /\ []
    "initStream" -> Just $ "initStream" /\ [ i "url" HT.urlArg ] /\ []
    "initCam" -> Just $ "initCam" /\ [ i "index" HT.ciArg ] /\ []
    "initScreen" -> Just $ "initScreen" /\ [] /\ []

    "render" -> Just $ "render" /\ [ i "target" HT.rtArg ] /\ []
    "update" -> Just $ "update" /\ [ i "update" HT.updateArg ] /\ []
    "setResolution" -> Just $ "setResolution" /\ [ i "width" HT.sideArg, i "height" HT.sideArg ] /\ []
    "hush" -> Just $ "hush" /\ [] /\ []
    "setFunction" -> Just $ "setFunction" /\ [ i "fn" HT.glslArg ] /\ []

    -- "speed" -> Synth
    -- "bpm" -> Synth
    "width" -> Just $ "width" /\ [] /\ [ o "out" HT.vOut ]
    "height" -> Just $ "height" /\ [] /\ [ o "out" HT.vOut ]
    "time" -> Just $ "time" /\ [] /\ [ o "out" HT.vOut ]
    "mouse" -> Just $ "time" /\ [] /\ [ o "x" HT.vOut, o "y" HT.vOut ]
    "pi" -> Just $ "time" /\ [] /\ [ o "out" HT.vOut ]

    "fft" -> Just $ "fft" /\ [ i "audio" HT.audioArg ] /\ [ o "out" HT.vOut ]
    "setSmooth" -> Just $ "setSmooth" /\ [ i "audio" HT.audioArg, i "smooth" $ HT.narg 0.4 ] /\ []
    "setCutoff" -> Just $ "setCutoff" /\ [ i "audio" HT.audioArg, i "cutoff" $ HT.narg 2.0 ] /\ []
    "setBins" -> Just $ "setCutoff" /\ [ i "audio" HT.audioArg, i "bins" HT.audioBinsArg ] /\ []
    "setScale" -> Just $ "setScale" /\ [ i "audio" HT.audioArg, i "scale" $ HT.narg 10.0 ] /\ []
    "hide" -> Just $ "hide" /\ [] /\ []
    "show" -> Just $ "show" /\ [] /\ []

    "out" -> Just $ "out" /\ [ i "output" HT.outputArg ] /\ []

    "linear" -> Just $ "linear" /\ [ i "array" HT.valuesArg ] /\ [ o "out" HT.arrOut ]
    "ease" -> Just $ "ease" /\ [ i "ease" HT.easeArg ] /\ [ o "out" HT.arrOut ]
    "fast" -> Just $ "fast" /\ [ i "array" HT.valuesArg, i "v" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]
    "smooth" -> Just $ "smooth" /\ [ i "array" HT.valuesArg, i "v" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]
    "offset" -> Just $ "offset" /\ [ i "array" HT.valuesArg, i "v" $ HT.narg 0.5 ] /\ [ o "out" HT.arrOut ]
    "fit" -> Just $ "fit" /\ [ i "array" HT.valuesArg, i "low" $ HT.narg 0.0, i "high" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]

    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing



-- instance PossiblyToFn Value Unit Fn.KnownFn where
--     possiblyToFn = Fn.nameOf >>> defaultsFor

-- instance FnDefault // FnDocs // FnTypes


-- TODO: probably duplicates something, is it used? replace with above instance of `defaultsFor`?`
-- TODO: private
-- TODO: generate from NDF
fromKnownFn :: String -> Maybe (Signature HT.Value Unit)
fromKnownFn = map sigs <<< case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ i "scale" $ HT.Number 10.0, i "offset" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "voronoi" -> Just $ "voronoi" /\ [ i "scale" $ HT.Number 5.0, i "speed" $ HT.Number 0.3, i "blending" $ HT.Number 0.3 ] /\ [ o "out" unit ]
    "osc" -> Just $ "osc" /\ [ i "frequency" $ HT.Number 60.0, i "sync" $ HT.Number 0.1, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "shape" -> Just $ "shape" /\ [ i "sides" $ HT.Number 3.0, i "radius" $ HT.Number 0.3, i "smoothing" $ HT.Number 0.01 ] /\ [ o "out" unit ]
    "gradient" -> Just $ "gradient" /\ [ i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    -- "src" -> Source
    "solid" -> Just $ "solid" /\ [ i "r" $ HT.Number 0.0, i "g" $ HT.Number 0.0, i "b" $ HT.Number 0.0, i "a" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ i "angle" $ HT.Number 10.0, i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scale" -> Just $ "scale" /\ [ i "amount" $ HT.Number 1.5, i "xMult" $ HT.Number 1.0, i "yMult" $ HT.Number 1.0, i "offsetX" $ HT.Number 0.5, i "offsetY" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "pixelate" -> Just $ "pixelate" /\ [ i "pixelX" $ HT.Number 20.0, i "pixelY" $ HT.Number 20.0 ] /\ [ o "out" unit ]
    "repeat" -> Just $ "repeat" /\ [ i "repeatX" $ HT.Number 3.0, i "repeatY" $ HT.Number 3.0, i "offsetX" $ HT.Number 0.0, i "offsetY" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "repeatX" -> Just $ "repeatX" /\ [ i "reps" $ HT.Number 3.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "repeatY" -> Just $ "repeatY" /\ [ i "reps" $ HT.Number 3.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "kaleid" -> Just $ "kaleid" /\ [ i "nSides" $ HT.Number 4.0 ] /\ [ o "out" unit ]
    "scroll" -> Just $ "scroll" /\ [ i "scrollX" $ HT.Number 0.5, i "scrollY" $ HT.Number 0.5, i "speedX" $ HT.Number 0.0, i "speedY" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scrollX" -> Just $ "scrollX" /\ [ i "scrollX" $ HT.Number 0.5, i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scrollY" -> Just $ "scrollY" /\ [ i "scrollY" $ HT.Number 0.5, i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]

    "posterize" -> Just $ "posterize" /\ [ i "bins" $ HT.Number 3.0, i "gamma" $ HT.Number 6.0 ] /\ [ o "out" unit ]
    "shift" -> Just $ "shift" /\ [ i "r" $ HT.Number 0.5, i "g" $ HT.Number 0.0, i "b" $ HT.Number 0.0, i "a" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "invert" -> Just $ "invert" /\ [ i "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "contrast" -> Just $ "contrast" /\ [ i "amount" $ HT.Number 1.6 ] /\ [ o "out" unit ]
    "brightness" -> Just $ "brightness" /\ [ i "amount" $ HT.Number 0.4 ] /\ [ o "out" unit ]
    "luma" -> Just $ "luma" /\ [ i "threshold" $ HT.Number 0.5, i "tolerance" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "thresh" -> Just $ "thresh" /\ [ i "threshold" $ HT.Number 0.5, i "tolerance" $ HT.Number 0.04 ] /\ [ o "out" unit ]
    "color" -> Just $ "color" /\ [ i "r" $ HT.Number 1.0, i "g" $ HT.Number 1.0, i "b" $ HT.Number 1.0, i "a" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "saturate" -> Just $ "saturate" /\ [ i "amount" $ HT.Number 2.0 ] /\ [ o "out" unit ]
    "hue" -> Just $ "hue" /\ [ i "amount" $ HT.Number 0.4 ] /\ [ o "out" unit ]
    "colorama" -> Just $ "colorama" /\ [ i "amount" $ HT.Number 0.005 ] /\ [ o "out" unit ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ i "scale" $ HT.Number 1.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "g" -> Just $ "g" /\ [ i "scale" $ HT.Number 1.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "b" -> Just $ "b" /\ [ i "scale" $ HT.Number 1.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "a" -> Just $ "a" /\ [ i "scale" $ HT.Number 1.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ i "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "sub" -> Just $ "sub" /\ [ i "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "layer" -> Just $ "layer" /\ [] /\ []
    "blend" -> Just $ "blend" /\ [ i "amount" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "mult" -> Just $ "mult" /\ [ i "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "diff" -> Just $ "diff" /\ [] /\ []
    "mask" -> Just $ "mask" /\ [] /\ []

    "modulateRepeat" -> Just $ "modRepeat" /\ [ i "repeatX" $ HT.Number 3.0, i "repeatY" $ HT.Number 3.0, i "offsetX" $ HT.Number 0.5, i "offsetY" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ i "reps" $ HT.Number 3.0, i "offset" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ i "reps" $ HT.Number 3.0, i "offset" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ i "nSides" $ HT.Number 4.0 ] /\ [ o "out" unit ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ i "scrollX" $ HT.Number 0.5, i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ i "scrollY" $ HT.Number 0.5, i "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulate" -> Just $ "modulate" /\ [ i "amount" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "modulateScale" -> Just $ "modScale" /\ [ i "multiple" $ HT.Number 1.0, i "offset" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ i "multiple" $ HT.Number 10.0, i "offset" $ HT.Number 3.0 ] /\ [ o "out" unit ]
    "modulateRotate" -> Just $ "modRotate" /\ [ i "multiple" $ HT.Number 1.0, i "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulateHue" -> Just $ "modHue" /\ [ i "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]

    -- "initCam" -> ExternalSources
    -- "initImage" -> ExternalSources
    -- "initVideo" -> ExternalSources
    -- "init" -> ExternalSources
    -- "initStream" -> ExternalSources
    -- "initScreen" -> ExternalSources

    -- "render" -> Synth
    -- "update" -> Synth
    -- "setResolution" -> Synth
    -- "hush" -> Synth
    -- "setFunction" -> Synth
    -- "speed" -> Synth
    -- "bpm" -> Synth
    -- "width" -> Synth
    -- "height" -> Synth
    -- "time" -> Synth
    -- "mouse" -> Synth
    -- "pi" -> Synth

    -- "fft" -> Audio
    -- "setSmooth" -> Audio
    -- "setCutoff" -> Audio
    -- "setBins" -> Audio
    -- "setScale" -> Audio
    -- "hide" -> Audio
    -- "show" -> Audio

    -- "setScale" -> Audio

    -- "out" -> Out

    "linear" -> Just $ "linear" /\ [] /\ [ o "out" unit ]
    "fast" -> Just $ "fast" /\ [ i "v" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "smooth" -> Just $ "smooth" /\ [ i "v" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "fit" -> Just $ "fit" /\ [ i "low" $ HT.Number 0.0, i "high" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "offset" -> Just $ "offset" /\ [ i "v" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing