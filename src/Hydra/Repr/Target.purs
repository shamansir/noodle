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
import Noodle.Fn.ToFn (Fn, class ToFn, toFn, class PossiblyToFn, possiblyToFn, q, o)
import Noodle.Fn.ToFn (Argument, Output, argName, argValue, empty) as Fn

import Hydra.Types


foreign import data HYDRA_V :: Target
-- Hydra value definition



hydraV :: _ HYDRA_V
hydraV = Proxy


instance ToCode HYDRA_V opts Int where toCode _ = const show
instance ToCode HYDRA_V opts Number where toCode _ = const show
instance ToCode HYDRA_V opts String where toCode _ = const show


_encode :: forall a. ToCode HYDRA_V Unit a => a -> String
_encode = toCode hydraV unit


instance ToCode HYDRA_V opts Value where
    toCode :: Proxy HYDRA_V -> opts -> Value -> String
    toCode _ _ = case _ of
        None -> "0 V"
        Undefined -> "U V"
        Number n -> "N " <> _encode n
        VArray vals ease -> "VA " <> _encode vals <> " $$ " <> _encode ease <> ""
        Dep fn -> "D " <> _encode fn
        Time -> "T V"
        MouseX -> "MX V"
        MouseY -> "MY V"
        Width -> "W V"
        Height -> "H V"
        Pi -> "PI V"
        Fft bin -> "A " <> _encode bin


instance ToCode HYDRA_V opts Texture where
    toCode :: Proxy HYDRA_V -> opts -> Texture -> String
    toCode _ _ = case _ of
        Empty -> "EMP T"
        Start src -> "S " <> _encode src
        BlendOf { what, with } blend -> "B " <> _encode what <> texSep <> _encode with <> texSep <> _encode blend <> texsEnd
        Filter texture op -> "F " <> _encode op <> texSep <> _encode texture <> texsEnd
        ModulateWith { what, with } mod -> "M " <> _encode what <> texSep <> _encode with <> texSep <> _encode mod <> texsEnd
        Geometry texture gmt -> "G " <> _encode texture <> texSep <> _encode gmt <> texsEnd
        CallGlslFn { over, mbWith } fn ->
            "CALL " <> _encode over <> texSep
                <> (
                    case mbWith of
                        Just with -> _encode with <> texSep
                        Nothing -> mempty
                )
                <> _encode fn <> texsEnd


instance ToCode HYDRA_V opts Blend where
    toCode :: Proxy HYDRA_V -> opts -> Blend -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts ColorOp where
    toCode :: Proxy HYDRA_V -> opts -> ColorOp -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts Modulate where
    toCode :: Proxy HYDRA_V -> opts -> Modulate -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts Geometry where
    toCode :: Proxy HYDRA_V -> opts -> Geometry -> String
    toCode _ _ = _encodeUsingFn


instance ToCode HYDRA_V opts TODO where
    toCode :: Proxy HYDRA_V -> opts -> TODO -> String
    toCode _ _ = const "TODO"


instance ToCode HYDRA_V opts Context where
    toCode :: Proxy HYDRA_V -> opts -> Context -> String
    toCode _ _ (Context { time }) = "{ " <> _encode time <> " }"


instance ToCode HYDRA_V opts UpdateFn where
    toCode :: Proxy HYDRA_V -> opts -> UpdateFn -> String
    toCode _ _ = const "UF" -- TODO


instance ToCode HYDRA_V opts ExtSource where
    toCode :: Proxy HYDRA_V -> opts -> ExtSource -> String
    toCode _ _ = case _ of
        Camera n -> "C " <> show n
        Sketch name -> "SK " <> name
        Video -> "V X"
        Unclear -> "U X"


instance ToCode HYDRA_V opts Source where
    toCode :: Proxy HYDRA_V -> opts -> Source -> String
    toCode _ _ = case _ of
        Load outputN -> "O " <> _encode outputN
        External sourceN def -> "X " <> _encode sourceN <> argSep <> _encode def <> argsEnd
        Gradient { speed } -> "G " <> _encode speed <> argsEnd
        Noise { scale, offset } -> "N " <> _encode scale <> argSep <> _encode offset <> argsEnd
        Osc { frequency, sync, offset } -> "OSC " <> _encode frequency <> argSep <> _encode sync <> argSep <> _encode offset <> argsEnd
        Shape { sides, radius, smoothing } -> "SHP " <> _encode sides <> argSep <> _encode radius <> argSep <> _encode smoothing <> argsEnd
        Solid { r, g, b, a } -> "S " <> _encode r <> argSep <> _encode g <> argSep <> _encode b <> argSep <> _encode a <> argsEnd
        Voronoi { scale, speed, blending } -> "V " <> _encode scale <> argSep <> _encode speed <> argSep <> _encode blending <> argsEnd


instance ToCode HYDRA_V opts RenderTarget where
    toCode :: Proxy HYDRA_V -> opts -> RenderTarget -> String
    toCode _ _ Four = "ALL"
    toCode _ _ (Output on) = show on


instance ToCode HYDRA_V opts Url where
    toCode :: Proxy HYDRA_V -> opts -> Url -> String
    toCode _ _ (Url url) = _encode url


instance ToCode HYDRA_V opts GlslFnKind where
    toCode :: Proxy HYDRA_V -> opts -> GlslFnKind -> String
    toCode _ _ = case _ of
        FnSrc -> "SRC"
        FnCoord -> "CRD"
        FnCombineCoord -> "CCR"
        FnCombine -> "CMB"
        FnColor -> "CLR"


instance ToCode HYDRA_V opts TOrV where
    toCode :: Proxy HYDRA_V -> opts -> TOrV -> String
    toCode _ _ = case _ of
        T tex -> "TT " <> _encode tex
        V val -> "VV " <> _encode val


instance ToCode HYDRA_V opts GlslFn where
    toCode :: Proxy HYDRA_V -> opts -> GlslFn -> String
    toCode _ _ (GlslFn (kind /\ GlslFnCode code /\ fn))
        = _encode kind <> " "
            <> glslStart <> _encode code <> glslEnd
            <> " " <> _encodeFnWithArgNames fn


instance ToCode HYDRA_V opts GlslFnRef where
    toCode :: Proxy HYDRA_V -> opts -> GlslFnRef -> String
    toCode _ _ (GlslFnRef fn)
        = _encodeFnWithArgNames fn


{- instance ToCode HYDRA_V opts (Lang.Fn TOrV) where
    toCode :: Proxy HYDRA_V -> opts -> Lang.Fn TOrV -> String
    _encode _ = "" -}


instance ToCode HYDRA_V opts CanBeSource where
    toCode :: Proxy HYDRA_V -> opts -> CanBeSource -> String
    toCode _ _ (CanBeSource cbs) =
        case cbs of
            Left sourceN -> "L " <> _encode sourceN
            Right outputN -> "R " <> _encode outputN


instance ToCode HYDRA_V opts SourceOptions where
    toCode :: Proxy HYDRA_V -> opts -> SourceOptions -> String
    toCode _ _ (SourceOptions { src }) = "SO " -- TODO: <> _encode src


instance ToCode HYDRA_V opts Values where
    toCode :: Proxy HYDRA_V -> opts -> Values -> String
    toCode _ _ (Values array) = "%% " <> String.joinWith " <> " (_encode <$> array) <> " %%"


instance ToCode HYDRA_V opts Ease where
    toCode :: Proxy HYDRA_V -> opts -> Ease -> String
    toCode _ _ = case _ of
        Linear -> "LIN E"
        Fast v -> "FST " <> _encode v
        Smooth v -> "SMT " <> _encode v
        Fit { low, high } -> "FIT " <> _encode low <> " < " <> _encode high
        Offset v -> "OFF " <> _encode v
        InOutCubic -> "IOC E"


instance ToCode HYDRA_V opts AudioSource where
    toCode :: Proxy HYDRA_V -> opts -> AudioSource -> String
    toCode _ _ = case _ of
        Silence -> "SIL"
        Mic -> "MIC"
        File -> "FIL"


instance ToCode HYDRA_V opts AudioBin where
    toCode :: Proxy HYDRA_V -> opts -> AudioBin -> String
    toCode _ _ (AudioBin n) = "@" <> show n


instance ToCode HYDRA_V opts OutputN where
    toCode :: Proxy HYDRA_V -> opts -> OutputN -> String
    toCode _ _ = case _ of
        Output0 -> "O0"
        Output1 -> "O1"
        Output2 -> "O2"
        Output3 -> "O3"
        Output4 -> "O4"


instance ToCode HYDRA_V opts SourceN where
    toCode :: Proxy HYDRA_V -> opts -> SourceN -> String
    toCode _ _ = case _ of
        Source0 -> "S0"


instance ToCode HYDRA_V opts JsExpr where
    toCode :: Proxy HYDRA_V -> opts -> JsExpr -> String
    toCode _ _ = case _ of
        Val value -> show value -- FIXME: for sure, not `encode`?
        AddE v1 v2 -> show v1 <> " + " <> show v2 -- FIXME: for sure, not `encode`?
        SubE v1 v2 -> show v1 <> " - " <> show v2 -- FIXME: for sure, not `encode`?
        MulE v1 v2 -> show v1 <> " * " <> show v2 -- FIXME: for sure, not `encode`?
        DivE v1 v2 -> show v1 <> " / " <> show v2 -- FIXME: for sure, not `encode`?
        ModE v1 v2 -> show v1 <> " % " <> show v2 -- FIXME: for sure, not `encode`?
        Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> show expr <> ")"
                    Nothing -> ""
                )
        Brackets expr -> "( " <> show expr <> " )"


instance ToCode HYDRA_V opts DepFn where
    toCode :: Proxy HYDRA_V -> opts -> DepFn -> String
    toCode _ _ = case _ of
        UserExpr jsexpr -> _encode jsexpr
        DepFn _ -> "[[CODE]]"
        Unparsed str -> unparsedFnStart <> str <> unparsedFnEnd -- "<<<< " <> str <> " >>>>"
        NoAction -> "/----/"


_encodeUsingFn :: forall a. ToFn Value Unit a => a -> String
_encodeUsingFn a =
    case toFn a :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit) of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                String.toUpper name <> " " <> String.joinWith argSep (_encode <$> Fn.argValue <$> args) <> argsEnd
            else
                String.toUpper name <> " " <> argsEnd


_encodeFnWithArgNames :: forall target opts arg out. ToCode HYDRA_V Unit arg => Fn arg out -> String
_encodeFnWithArgNames fn =
    case (toFn fn :: String /\ Array (Fn.Argument arg) /\ Array (Fn.Output out)) of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                name <> " " <> show (Array.length args) <> " " <> String.joinWith argSep (_encodeArg <$> args) <> argsEnd
            else
                name <> " " <> show (Array.length args) <> " " <> argsEnd
    where
        _encodeArg arg =
            Fn.argName arg <> "::" <> _encode (Fn.argValue arg)
    -- Fn.name fn
