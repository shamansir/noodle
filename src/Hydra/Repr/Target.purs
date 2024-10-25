module Hydra.Repr.Target where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith, toUpper) as String
import Data.Array (length) as Array

import Type.Proxy (Proxy(..))

import Noodle.Text.Code.Target (Target)
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.FromCode (class CanParse, class FromCode, fromCode, fromParser, SourceError)
import Noodle.Fn.ToFn (Fn, class ToFn, toFn, class PossiblyToFn, possiblyToFn, q, o)
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
    toCode _ _ HT.Four = "ALL"
    toCode _ _ (HT.Output on) = show on


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


instance ToCode HYDRA_V opts HT.GlslFn where
    toCode :: Proxy HYDRA_V -> opts -> HT.GlslFn -> String
    toCode _ _ (HT.GlslFn (kind /\ HT.GlslFnCode code /\ fn))
        = _encode kind <> " "
            <> PM._glslStart <> _encode code <> PM._glslEnd
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
    toCode _ _ (HT.Values array) = "%% " <> String.joinWith " <> " (_encode <$> array) <> " %%"


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
    toCode _ _ = case _ of
        HT.Val value -> show value -- FIXME: for sure, not `encode`?
        HT.AddE v1 v2 -> show v1 <> " + " <> show v2 -- FIXME: for sure, not `encode`?
        HT.SubE v1 v2 -> show v1 <> " - " <> show v2 -- FIXME: for sure, not `encode`?
        HT.MulE v1 v2 -> show v1 <> " * " <> show v2 -- FIXME: for sure, not `encode`?
        HT.DivE v1 v2 -> show v1 <> " / " <> show v2 -- FIXME: for sure, not `encode`?
        HT.ModE v1 v2 -> show v1 <> " % " <> show v2 -- FIXME: for sure, not `encode`?
        HT.Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> show expr <> ")"
                    Nothing -> ""
                )
        HT.Brackets expr -> "( " <> show expr <> " )"


instance ToCode HYDRA_V opts HT.DepFn where
    toCode :: Proxy HYDRA_V -> opts -> HT.DepFn -> String
    toCode _ _ = case _ of
        HT.UserExpr jsexpr -> _encode jsexpr
        HT.DepFn _ -> "[[CODE]]"
        HT.Unparsed str -> PM._unparsedFnStart <> str <> PM._unparsedFnEnd -- "<<<< " <> str <> " >>>>"
        HT.NoAction -> "/----/"


_encodeUsingFn :: forall a. ToFn HT.Value Unit a => a -> String
_encodeUsingFn a =
    case toFn a :: String /\ Array (Fn.Argument HT.Value) /\ Array (Fn.Output Unit) of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                String.toUpper name <> " " <> String.joinWith PM._argSep (_encode <$> Fn.argValue <$> args) <> PM._argsEnd
            else
                String.toUpper name <> " " <> PM._argsEnd


_encodeFnWithArgNames :: forall arg out. ToCode HYDRA_V Unit arg => Fn arg out -> String
_encodeFnWithArgNames fn =
    case (toFn fn :: String /\ Array (Fn.Argument arg) /\ Array (Fn.Output out)) of
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


instance FromCode HYDRA_V opts HT.Value    where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Texture  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Source   where fromCode = fromParser
instance FromCode HYDRA_V opts HT.OutputN  where fromCode = fromParser
instance FromCode HYDRA_V opts HT.AudioBin where fromCode = fromParser
instance FromCode HYDRA_V opts HT.TODO     where fromCode = fromParser
instance FromCode HYDRA_V opts HT.Values   where fromCode = fromParser
instance FromCode HYDRA_V opts HT.GlslFn   where fromCode = fromParser