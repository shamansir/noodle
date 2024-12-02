module Hydra.Repr.Target where

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
import Noodle.Fn.ToFn (Fn, FnS, class ToFn, fns, toFn, class PossiblyToFn, possiblyToFn, q, o)
import Noodle.Fn.ToFn (Argument, Output, argName, argValue, empty) as Fn

import Hydra.Types as HT
import Hydra.Repr.Parser as RP
import Hydra.Repr.Markers as PM


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


_encodeUsingFn :: forall a. ToFn HYDRA_V HT.Value Unit a => a -> String
_encodeUsingFn a =
    case unwrap $ toFn hydraV a :: FnS HT.Value Unit of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                String.toUpper name <> " " <> String.joinWith PM._argSep (_encode <$> Fn.argValue <$> args) <> PM._argsEnd
            else
                String.toUpper name <> " " <> PM._argsEnd


_encodeFnWithArgNames :: forall arg out. ToCode HYDRA_V Unit arg => Fn arg out -> String
_encodeFnWithArgNames fn =
    case unwrap $ toFn (Proxy :: _ Void) fn :: FnS arg out of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                name <> " " <> show (Array.length args) <> " " <> String.joinWith PM._argSep (_encodeArg <$> args) <> PM._argsEnd
            else
                name <> " " <> show (Array.length args) <> " " <> PM._argsEnd
    where
        _encodeArg arg =
            Fn.argName arg <> "::" <> _encode (Fn.argValue arg)
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
instance ToFn HYDRA_V HT.Value Unit HT.ColorOp where
    toFn :: Proxy _ -> HT.ColorOp -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.R { scale, offset } -> "r" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        HT.G { scale, offset } -> "g" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        HT.B { scale, offset } -> "b" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        HT.A { scale, offset } -> "a" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        HT.Posterize { bins, gamma } -> "posterize" /\ [ q "bins" bins, q "gamma" gamma ] /\ [ o "out" unit ]
        HT.Shift { r, g, b, a } -> "shift" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        HT.Invert amount -> "invert" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Contrast amount -> "contrast" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Brightness amount -> "brightness" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Luma { threshold, tolerance } -> "luma" /\ [ q "threshold" threshold, q "tolerance" tolerance ] /\ [ o "out" unit ]
        HT.Thresh { threshold, tolerance } -> "thresh" /\ [ q "threshold" threshold, q "tolerance" tolerance ] /\ [ o "out" unit ]
        HT.Color { r, g, b, a } -> "color" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        HT.Saturate amount -> "saturate" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Hue amount -> "hue" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Colorama amount -> "colorama" /\ [ q "amount" amount ] /\ [ o "out" unit ]


-- TODO: generate it automatically from NDF
instance ToFn HYDRA_V HT.Value Unit HT.Modulate where
    toFn :: Proxy _ -> HT.Modulate -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.Modulate amount -> "modulate" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.ModHue amount -> "modHue" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.ModKaleid { nSides } -> "modKaleid" /\ [ q "nSides" nSides ] /\ [ o "out" unit ]
        HT.ModPixelate { multiple, offset } -> "modPixelate" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        HT.ModRepeat { repeatX, repeatY, offsetX, offsetY } -> "modRepeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.ModRepeatX { reps, offset } -> "modRepeatX" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        HT.ModRepeatY { reps, offset } -> "modRepeatY" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        HT.ModRotate { multiple, offset } -> "modRotate" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        HT.ModScale { multiple, offset } -> "modScale" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        HT.ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ] /\ [ o "out" unit ]
        HT.ModScrollX { scrollX, speed } -> "modScrollX" /\ [ q "scrollX" scrollX, q "speed" speed ] /\ [ o "out" unit ]
        HT.ModScrollY { scrollY, speed } -> "modScrollY" /\ [ q "scrollY" scrollY, q "speed" speed ] /\ [ o "out" unit ]


instance ToFn HYDRA_V HT.Value Unit HT.Blend where
    toFn :: Proxy _ -> HT.Blend -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.Blend amount -> "blend" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Add amount -> "add" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Sub amount -> "sub" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Mult amount -> "mult" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Layer amount -> "layer" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        HT.Diff -> "diff" /\ [] /\ []
        HT.Mask -> "mask" /\ [] /\ []


instance ToFn HYDRA_V HT.Value Unit HT.Geometry where
    toFn :: Proxy _ -> HT.Geometry -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.GKaleid { nSides } -> "kaleid" /\ [ q "nSides" nSides ] /\ [ o "out" unit ]
        HT.GPixelate { pixelX, pixelY } -> "pixelate" /\ [ q "pixelX" pixelX, q "pixelY" pixelY ] /\ [ o "out" unit ]
        HT.GRepeat { repeatX, repeatY, offsetX, offsetY } -> "repeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.GRepeatX { reps, offset } -> "repeatX" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        HT.GRepeatY { reps, offset } -> "repeatY" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        HT.GRotate { angle, speed } -> "rotate" /\ [ q "angle" angle, q "speed" speed ] /\ [ o "out" unit ]
        HT.GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ q "amount" amount, q "xMult" xMult, q "yMult" yMult, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        HT.GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ] /\ [ o "out" unit ]
        HT.GScrollX { scrollX, speed } -> "scrollX" /\ [ q "scrollX" scrollX, q "speed" speed ] /\ [ o "out" unit ]
        HT.GScrollY { scrollY, speed } -> "scrollY" /\ [ q "scrollY" scrollY, q "speed" speed ] /\ [ o "out" unit ]


instance ToFn HYDRA_V HT.Value Unit HT.Ease where
    toFn :: Proxy _ -> HT.Ease -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.Linear -> "linear" /\ [] /\ [ o "out" unit ]
        HT.Fast v -> "fast" /\ [ q "v" v ] /\ [ o "out" unit ]
        HT.Smooth v -> "smooth" /\ [ q "v" v ] /\ [ o "out" unit ]
        HT.Fit { low, high } -> "fit" /\ [ q "low" low, q "high" high ] /\ [ o "out" unit ]
        HT.Offset v -> "offset" /\ [ q "v" v ] /\ [ o "out" unit ]
        HT.InOutCubic -> "inOutCubic" /\ [] /\ [ o "out" unit ]


instance ToFn HYDRA_V HT.GlslFnArg HT.GlslFnOut HT.GlslFn where
    toFn :: Proxy _ -> HT.GlslFn -> Fn HT.GlslFnArg HT.GlslFnOut
    toFn ph (HT.GlslFn { fn }) = toFn (Proxy :: _ Void) fn


instance ToFn HYDRA_V HT.GlslFnArg HT.GlslFnOut HT.GlslFnRef where
    toFn :: Proxy _ -> HT.GlslFnRef -> Fn HT.GlslFnArg HT.GlslFnOut
    toFn ph (HT.GlslFnRef fn) = toFn (Proxy :: _ Void) fn


instance ToFn HYDRA_V HT.Value Unit HT.From where
    toFn :: Proxy _ -> HT.From -> Fn HT.Value Unit
    toFn = const $ fns <<< case _ of
        HT.Gradient { speed } -> "gradient" /\ [ q "speed" speed ] /\ [ o "out" unit ]
        HT.Noise { scale, offset } -> "noise" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        HT.Osc { frequency, sync, offset } -> "osc" /\ [ q "frequency" frequency, q "sync" sync, q "offset" offset ] /\ [ o "out" unit ]
        HT.Shape { sides, radius, smoothing } -> "shape" /\ [ q "sides" sides, q "radius" radius, q "smoothing" smoothing ] /\ [ o "out" unit ]
        HT.Solid { r, g, b, a } -> "solid" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        HT.Voronoi { scale, speed, blending } -> "voronoi" /\ [ q "scale" scale, q "speed" speed, q "blending" blending ] /\ [ o "out" unit ]


instance PossiblyToFn HYDRA_V HT.Value Unit HT.Source where
    possiblyToFn :: Proxy _ -> HT.Source -> Maybe (Fn HT.Value Unit)
    possiblyToFn ph = case _ of
        HT.Load outputN -> Nothing -- TODO: could be converted to `src()`
        HT.External sourceN ext -> Nothing -- TODO: could be converted to `src()` ?
        HT.From from -> Just $ toFn ph from


instance PossiblyToFn HYDRA_V HT.TOrV HT.OTOrV HT.Texture where
    possiblyToFn :: Proxy _ -> HT.Texture -> Maybe (Fn HT.TOrV HT.OTOrV)
    possiblyToFn ph = case _ of
        HT.Empty -> Nothing
        HT.Start src ->
            case (unwrap <$> possiblyToFn ph src :: Maybe (FnS HT.Value Unit)) of
                Just (name /\ args /\ outs) -> Just $ fns $ name /\ (map HT.V <$> args) /\ (map (const HT.OT) <$> outs)
                Nothing -> Nothing
        HT.BlendOf { what, with } blend ->
            case unwrap $ toFn ph blend :: FnS HT.Value Unit of
                name /\ args /\ outs -> Just $ fns $ name /\ ((q "what" $ HT.T what) : (map HT.V <$> args) <> [ q "with" $ HT.T with ]) /\ (map (const HT.OT) <$> outs)
        HT.Filter texture cop ->
            case unwrap $ toFn ph cop :: FnS HT.Value Unit of
                name /\ args /\ outs -> Just $ fns $ name /\ ((map HT.V <$> args) <> [ q "texture" $ HT.T texture ]) /\ (map (const HT.OT) <$> outs)
        HT.ModulateWith { what, with } mod ->
            case unwrap $ toFn ph mod :: FnS HT.Value Unit of
                name /\ args /\ outs -> Just $ fns $ name /\ ((q "what" $ HT.T what) : (map HT.V <$> args) <> [ q "with" $ HT.T with ]) /\ (map (const HT.OT) <$> outs)
        HT.Geometry texture gmt ->
            case unwrap $ toFn ph gmt :: FnS HT.Value Unit of
                name /\ args /\ outs -> Just $ fns $ name /\ ((map HT.V <$> args) <> [ q "texture" $ HT.T texture ]) /\ (map (const HT.OT) <$> outs)
        HT.CallGlslFn { over, mbWith } fnRef ->
            case unwrap $ toFn ph fnRef :: FnS HT.TOrV HT.GlslFnOut of
                name /\ args /\ outs -> Just $ fns $ name /\ ((q "over" $ HT.T over) : args <>
                    case mbWith of
                        Just with -> [ q "with" $ HT.T with ]
                        Nothing -> [ ]
                    ) /\ (map (const HT.OT) <$> outs)


instance PossiblyToFn HYDRA_V HT.Value Unit HT.HydraFnId where
    possiblyToFn :: Proxy _ -> HT.HydraFnId -> Maybe (Fn HT.Value Unit)
    possiblyToFn _ = unwrap >>> fromKnownFn


instance PossiblyToFn HYDRA_V HT.FnArg HT.FnOut HT.HydraFnId where
    possiblyToFn _ = unwrap >>> defaultsFor


-- TODO: generate from NDF
defaultsFor :: String -> Maybe (Fn HT.FnArg HT.FnOut) -- TODO: output of Fn!
-- defaultValuesOf :: FamilyR -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
defaultsFor = map fns <<< case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ q "scale" $ HT.narg 10.0, q "offset" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "voronoi" -> Just $ "voronoi" /\ [ q "scale" $ HT.narg 5.0, q "speed" $ HT.narg 0.3, q "blending" $ HT.narg 0.3 ] /\ [ o "out" HT.tOut ]
    "osc" -> Just $ "osc" /\ [ q "frequency" $ HT.narg 60.0, q "sync" $ HT.narg 0.1, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "shape" -> Just $ "shape" /\ [ q "sides" $ HT.narg 3.0, q "radius" $ HT.narg 0.3, q "smoothing" $ HT.narg 0.01 ] /\ [ o "out" HT.tOut ]
    "gradient" -> Just $ "gradient" /\ [ q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "src" -> Just $ "src" /\ [ q "src" HT.sourceArg  ] /\ [ o "out" HT.tOut ]
    "solid" -> Just $ "solid" /\ [ q "r" $ HT.narg 0.0, q "g" $ HT.narg 0.0, q "b" $ HT.narg 0.0, q "a" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ q "what" HT.tArg, q "angle" $ HT.narg 10.0, q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scale" -> Just $ "scale" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 1.5, q "xMult" $ HT.narg 1.0, q "yMult" $ HT.narg 1.0, q "offsetX" $ HT.narg 0.5, q "offsetY" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "pixelate" -> Just $ "pixelate" /\ [ q "what" HT.tArg, q "pixelX" $ HT.narg 20.0, q "pixelY" $ HT.narg 20.0 ] /\ [ o "out" HT.tOut ]
    "repeat" -> Just $ "repeat" /\ [ q "what" HT.tArg, q "repeatX" $ HT.narg 3.0, q "repeatY" $ HT.narg 3.0, q "offsetX" $ HT.narg 0.0, q "offsetY" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "repeatX" -> Just $ "repeatX" /\ [ q "what" HT.tArg, q "reps" $ HT.narg 3.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "repeatY" -> Just $ "repeatY" /\ [ q "what" HT.tArg, q "reps" $ HT.narg 3.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "kaleid" -> Just $ "kaleid" /\ [ q "what" HT.tArg, q "nSides" $ HT.narg 4.0 ] /\ [ o "out" HT.tOut ]
    "scroll" -> Just $ "scroll" /\ [ q "what" HT.tArg, q "scrollX" $ HT.narg 0.5, q "scrollY" $ HT.narg 0.5, q "speedX" $ HT.narg 0.0, q "speedY" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scrollX" -> Just $ "scrollX" /\ [ q "what" HT.tArg, q "scrollX" $ HT.narg 0.5, q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "scrollY" -> Just $ "scrollY" /\ [ q "what" HT.tArg, q "scrollY" $ HT.narg 0.5, q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]

    "posterize" -> Just $ "posterize" /\ [ q "what" HT.tArg, q "bins" $ HT.narg 3.0, q "gamma" $ HT.narg 6.0 ] /\ [ o "out" HT.tOut ]
    "shift" -> Just $ "shift" /\ [ q "what" HT.tArg, q "r" $ HT.narg 0.5, q "g" $ HT.narg 0.0, q "b" $ HT.narg 0.0, q "a" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "invert" -> Just $ "invert" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "contrast" -> Just $ "contrast" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 1.6 ] /\ [ o "out" HT.tOut ]
    "brightness" -> Just $ "brightness" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 0.4 ] /\ [ o "out" HT.tOut ]
    "luma" -> Just $ "luma" /\ [ q "what" HT.tArg, q "threshold" $ HT.narg 0.5, q "tolerance" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "thresh" -> Just $ "thresh" /\ [ q "what" HT.tArg, q "threshold" $ HT.narg 0.5, q "tolerance" $ HT.narg 0.04 ] /\ [ o "out" HT.tOut ]
    "color" -> Just $ "color" /\ [ q "what" HT.tArg, q "r" $ HT.narg 1.0, q "g" $ HT.narg 1.0, q "b" $ HT.narg 1.0, q "a" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "saturate" -> Just $ "saturate" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 2.0 ] /\ [ o "out" HT.tOut ]
    "hue" -> Just $ "hue" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 0.4 ] /\ [ o "out" HT.tOut ]
    "colorama" -> Just $ "colorama" /\ [ q "what" HT.tArg, q "amount" $ HT.narg 0.005 ] /\ [ o "out" HT.tOut ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ q "what" HT.tArg, q "scale" $ HT.narg 1.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "g" -> Just $ "g" /\ [ q "what" HT.tArg, q "scale" $ HT.narg 1.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "b" -> Just $ "b" /\ [ q "what" HT.tArg, q "scale" $ HT.narg 1.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "a" -> Just $ "a" /\ [ q "what" HT.tArg, q "scale" $ HT.narg 1.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "sub" -> Just $ "sub" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "layer" -> Just $ "layer" /\ [ q "what" HT.tArg, q "with" HT.tArg ] /\ [ o "out" HT.tOut ]
    "blend" -> Just $ "blend" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "mult" -> Just $ "mult" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "diff" -> Just $ "diff" /\ [ q "what" HT.tArg, q "with" HT.tArg ] /\ [ o "out" HT.tOut ]
    "mask" -> Just $ "mask" /\ [ q "what" HT.tArg, q "with" HT.tArg ] /\ [ o "out" HT.tOut ]

    "modulateRepeat" -> Just $ "modRepeat" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "repeatX" $ HT.narg 3.0, q "repeatY" $ HT.narg 3.0, q "offsetX" $ HT.narg 0.5, q "offsetY" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "reps" $ HT.narg 3.0, q "offset" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "reps" $ HT.narg 3.0, q "offset" $ HT.narg 0.5 ] /\ [ o "out" HT.tOut ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "nSides" $ HT.narg 4.0 ] /\ [ o "out" HT.tOut ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "scrollX" $ HT.narg 0.5, q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "scrollY" $ HT.narg 0.5, q "speed" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulate" -> Just $ "modulate" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 0.1 ] /\ [ o "out" HT.tOut ]
    "modulateScale" -> Just $ "modScale" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "multiple" $ HT.narg 1.0, q "offset" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "multiple" $ HT.narg 10.0, q "offset" $ HT.narg 3.0 ] /\ [ o "out" HT.tOut ]
    "modulateRotate" -> Just $ "modRotate" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "multiple" $ HT.narg 1.0, q "offset" $ HT.narg 0.0 ] /\ [ o "out" HT.tOut ]
    "modulateHue" -> Just $ "modHue" /\ [ q "what" HT.tArg, q "with" HT.tArg, q "amount" $ HT.narg 1.0 ] /\ [ o "out" HT.tOut ]

    "initCam" -> Just $ "initCam" /\ [] /\ []
    "initImage" -> Just $ "initImage" /\ [ q "url" HT.urlArg ] /\ []
    "initVideo" -> Just $ "initVideo" /\ [ q "url" HT.urlArg ] /\ []
    "init" -> Just $ "init" /\ [ q "options" HT.optionsArg ] /\ []
    "initStream" -> Just $ "initStream" /\ [ q "url" HT.urlArg ] /\ []
    "initCam" -> Just $ "initCam" /\ [ q "index" HT.ciArg ] /\ []
    "initScreen" -> Just $ "initScreen" /\ [] /\ []

    "render" -> Just $ "render" /\ [ q "target" HT.rtArg ] /\ []
    "update" -> Just $ "update" /\ [ q "update" HT.updateArg ] /\ []
    "setResolution" -> Just $ "setResolution" /\ [ q "width" HT.sideArg, q "height" HT.sideArg ] /\ []
    "hush" -> Just $ "hush" /\ [] /\ []
    "setFunction" -> Just $ "setFunction" /\ [ q "fn" HT.glslArg ] /\ []

    -- "speed" -> Synth
    -- "bpm" -> Synth
    "width" -> Just $ "width" /\ [] /\ [ o "out" HT.vOut ]
    "height" -> Just $ "height" /\ [] /\ [ o "out" HT.vOut ]
    "time" -> Just $ "time" /\ [] /\ [ o "out" HT.vOut ]
    "mouse" -> Just $ "time" /\ [] /\ [ o "x" HT.vOut, o "y" HT.vOut ]
    "pi" -> Just $ "time" /\ [] /\ [ o "out" HT.vOut ]

    "fft" -> Just $ "fft" /\ [ q "audio" HT.audioArg ] /\ [ o "out" HT.vOut ]
    "setSmooth" -> Just $ "setSmooth" /\ [ q "audio" HT.audioArg, q "smooth" $ HT.narg 0.4 ] /\ []
    "setCutoff" -> Just $ "setCutoff" /\ [ q "audio" HT.audioArg, q "cutoff" $ HT.narg 2.0 ] /\ []
    "setBins" -> Just $ "setCutoff" /\ [ q "audio" HT.audioArg, q "bins" HT.audioBinsArg ] /\ []
    "setScale" -> Just $ "setScale" /\ [ q "audio" HT.audioArg, q "scale" $ HT.narg 10.0 ] /\ []
    "hide" -> Just $ "hide" /\ [] /\ []
    "show" -> Just $ "show" /\ [] /\ []

    "out" -> Just $ "out" /\ [ q "output" HT.outputArg ] /\ []

    "linear" -> Just $ "linear" /\ [ q "array" HT.valuesArg ] /\ [ o "out" HT.arrOut ]
    "ease" -> Just $ "ease" /\ [ q "ease" HT.easeArg ] /\ [ o "out" HT.arrOut ]
    "fast" -> Just $ "fast" /\ [ q "array" HT.valuesArg, q "v" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]
    "smooth" -> Just $ "smooth" /\ [ q "array" HT.valuesArg, q "v" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]
    "offset" -> Just $ "offset" /\ [ q "array" HT.valuesArg, q "v" $ HT.narg 0.5 ] /\ [ o "out" HT.arrOut ]
    "fit" -> Just $ "fit" /\ [ q "array" HT.valuesArg, q "low" $ HT.narg 0.0, q "high" $ HT.narg 1.0 ] /\ [ o "out" HT.arrOut ]

    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing



-- instance PossiblyToFn Value Unit Fn.KnownFn where
--     possiblyToFn = Fn.nameOf >>> defaultsFor

-- instance FnDefault // FnDocs // FnTypes


-- TODO: probably duplicates something, is it used? replace with above instance of `defaultsFor`?`
-- TODO: private
-- TODO: generate from NDF
fromKnownFn :: String -> Maybe (Fn HT.Value Unit)
fromKnownFn = map fns <<< case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ q "scale" $ HT.Number 10.0, q "offset" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "voronoi" -> Just $ "voronoi" /\ [ q "scale" $ HT.Number 5.0, q "speed" $ HT.Number 0.3, q "blending" $ HT.Number 0.3 ] /\ [ o "out" unit ]
    "osc" -> Just $ "osc" /\ [ q "frequency" $ HT.Number 60.0, q "sync" $ HT.Number 0.1, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "shape" -> Just $ "shape" /\ [ q "sides" $ HT.Number 3.0, q "radius" $ HT.Number 0.3, q "smoothing" $ HT.Number 0.01 ] /\ [ o "out" unit ]
    "gradient" -> Just $ "gradient" /\ [ q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    -- "src" -> Source
    "solid" -> Just $ "solid" /\ [ q "r" $ HT.Number 0.0, q "g" $ HT.Number 0.0, q "b" $ HT.Number 0.0, q "a" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ q "angle" $ HT.Number 10.0, q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scale" -> Just $ "scale" /\ [ q "amount" $ HT.Number 1.5, q "xMult" $ HT.Number 1.0, q "yMult" $ HT.Number 1.0, q "offsetX" $ HT.Number 0.5, q "offsetY" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "pixelate" -> Just $ "pixelate" /\ [ q "pixelX" $ HT.Number 20.0, q "pixelY" $ HT.Number 20.0 ] /\ [ o "out" unit ]
    "repeat" -> Just $ "repeat" /\ [ q "repeatX" $ HT.Number 3.0, q "repeatY" $ HT.Number 3.0, q "offsetX" $ HT.Number 0.0, q "offsetY" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "repeatX" -> Just $ "repeatX" /\ [ q "reps" $ HT.Number 3.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "repeatY" -> Just $ "repeatY" /\ [ q "reps" $ HT.Number 3.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "kaleid" -> Just $ "kaleid" /\ [ q "nSides" $ HT.Number 4.0 ] /\ [ o "out" unit ]
    "scroll" -> Just $ "scroll" /\ [ q "scrollX" $ HT.Number 0.5, q "scrollY" $ HT.Number 0.5, q "speedX" $ HT.Number 0.0, q "speedY" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scrollX" -> Just $ "scrollX" /\ [ q "scrollX" $ HT.Number 0.5, q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "scrollY" -> Just $ "scrollY" /\ [ q "scrollY" $ HT.Number 0.5, q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]

    "posterize" -> Just $ "posterize" /\ [ q "bins" $ HT.Number 3.0, q "gamma" $ HT.Number 6.0 ] /\ [ o "out" unit ]
    "shift" -> Just $ "shift" /\ [ q "r" $ HT.Number 0.5, q "g" $ HT.Number 0.0, q "b" $ HT.Number 0.0, q "a" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "invert" -> Just $ "invert" /\ [ q "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "contrast" -> Just $ "contrast" /\ [ q "amount" $ HT.Number 1.6 ] /\ [ o "out" unit ]
    "brightness" -> Just $ "brightness" /\ [ q "amount" $ HT.Number 0.4 ] /\ [ o "out" unit ]
    "luma" -> Just $ "luma" /\ [ q "threshold" $ HT.Number 0.5, q "tolerance" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "thresh" -> Just $ "thresh" /\ [ q "threshold" $ HT.Number 0.5, q "tolerance" $ HT.Number 0.04 ] /\ [ o "out" unit ]
    "color" -> Just $ "color" /\ [ q "r" $ HT.Number 1.0, q "g" $ HT.Number 1.0, q "b" $ HT.Number 1.0, q "a" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "saturate" -> Just $ "saturate" /\ [ q "amount" $ HT.Number 2.0 ] /\ [ o "out" unit ]
    "hue" -> Just $ "hue" /\ [ q "amount" $ HT.Number 0.4 ] /\ [ o "out" unit ]
    "colorama" -> Just $ "colorama" /\ [ q "amount" $ HT.Number 0.005 ] /\ [ o "out" unit ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ q "scale" $ HT.Number 1.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "g" -> Just $ "g" /\ [ q "scale" $ HT.Number 1.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "b" -> Just $ "b" /\ [ q "scale" $ HT.Number 1.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "a" -> Just $ "a" /\ [ q "scale" $ HT.Number 1.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ q "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "sub" -> Just $ "sub" /\ [ q "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "layer" -> Just $ "layer" /\ [] /\ []
    "blend" -> Just $ "blend" /\ [ q "amount" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "mult" -> Just $ "mult" /\ [ q "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "diff" -> Just $ "diff" /\ [] /\ []
    "mask" -> Just $ "mask" /\ [] /\ []

    "modulateRepeat" -> Just $ "modRepeat" /\ [ q "repeatX" $ HT.Number 3.0, q "repeatY" $ HT.Number 3.0, q "offsetX" $ HT.Number 0.5, q "offsetY" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ q "reps" $ HT.Number 3.0, q "offset" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ q "reps" $ HT.Number 3.0, q "offset" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ q "nSides" $ HT.Number 4.0 ] /\ [ o "out" unit ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ q "scrollX" $ HT.Number 0.5, q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ q "scrollY" $ HT.Number 0.5, q "speed" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulate" -> Just $ "modulate" /\ [ q "amount" $ HT.Number 0.1 ] /\ [ o "out" unit ]
    "modulateScale" -> Just $ "modScale" /\ [ q "multiple" $ HT.Number 1.0, q "offset" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ q "multiple" $ HT.Number 10.0, q "offset" $ HT.Number 3.0 ] /\ [ o "out" unit ]
    "modulateRotate" -> Just $ "modRotate" /\ [ q "multiple" $ HT.Number 1.0, q "offset" $ HT.Number 0.0 ] /\ [ o "out" unit ]
    "modulateHue" -> Just $ "modHue" /\ [ q "amount" $ HT.Number 1.0 ] /\ [ o "out" unit ]

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
    "fast" -> Just $ "fast" /\ [ q "v" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "smooth" -> Just $ "smooth" /\ [ q "v" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "fit" -> Just $ "fit" /\ [ q "low" $ HT.Number 0.0, q "high" $ HT.Number 1.0 ] /\ [ o "out" unit ]
    "offset" -> Just $ "offset" /\ [ q "v" $ HT.Number 0.5 ] /\ [ o "out" unit ]
    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing