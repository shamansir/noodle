module Toolkit.Hydra.Lang.ToCode where

import Prelude hiding (show)
import Prelude (show) as Core

import Toolkit.Hydra.Types
import Toolkit.Hydra.Repr.Wrap (WrapRepr)
import Toolkit.Hydra.Lang.Fn (toFnX, name, args, Argument(..))

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))


import Data.Text.Format as T


data Target


-- could they be split into different files?
-- TODO: NDF format is not bound to Hydra, rather to Noodle Engine, move it to external module
foreign import data JS :: Target
foreign import data JS_DISPLAY :: Target -- TODO: replace with `ToTaggedCode JS` later
foreign import data PS :: Target
foreign import data NDF :: Target


class ToCode (target :: Target) a where
    toCode :: Proxy target -> a -> String


class ToTaggedCode (target :: Target) a where
    toTaggedCode :: Proxy target -> a -> T.Tag


pureScript :: _ PS
pureScript = Proxy


javaScript :: _ JS
javaScript = Proxy


javaScriptToDisplay :: _ JS_DISPLAY
javaScriptToDisplay = Proxy


ndf :: _ NDF
ndf = Proxy


nodes :: _ NDF
nodes = Proxy


fnPs :: forall a. ToCode PS a => String -> Array a -> String
fnPs name vals = fnsPs name $ toCode pureScript <$> vals


fnsPs :: String -> Array String -> String
fnsPs name vals = name <> " " <> (String.joinWith " " vals)


fnePs :: String -> String
fnePs name = name


fnJs :: forall a. ToCode JS a => String -> Array a -> String
fnJs name vals = fnsJs name $ toCode javaScript <$> vals


fnsJs :: String -> Array String -> String
fnsJs name vals = name <> "( " <> (String.joinWith ", " vals) <> " )"


fneJs :: String -> String
fneJs name = name <> "()"


{- PURESCRIPT -}


instance ToCode PS Value where
    toCode :: Proxy PS -> Value -> String
    toCode _ = case _ of
        None -> "{- none -}"
        Undefined -> "{- ?? -}"
        Number n -> "(n " <> Core.show n <> ")"
        VArray vals ease -> toCode pureScript vals <> "\n\t# " <> toCode pureScript ease
        Dep _ -> "{- dep-fn -}"
        Time -> "time"
        MouseX -> "mouseX"
        MouseY -> "mouseY"
        Width -> "width"
        Height -> "height"
        Pi -> "pi"
        Fft bin -> "a # fft " <> toCode pureScript bin
        -- Audio audio bin -> toCode pureScript audio <> " # fft " <> toCode pureScript bin


instance ToCode PS Source where
    toCode :: Proxy PS -> Source -> String
    toCode _ = case _ of
        Load outputN -> toCode pureScript outputN
        External sourceN _ -> toCode pureScript sourceN
        -- FIXME: use PossiblyToFn
        Gradient { speed } -> fnPs "gradient" [ speed ]
        Noise { scale, offset } -> fnPs "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fnPs "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fnPs "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fnPs "solid" [ r, g, b, a ]
        Voronoi { scale, speed, blending } -> fnPs "voronoi" [ scale, speed, blending ]



instance ToCode PS ColorOp where
    toCode :: Proxy PS -> ColorOp -> String
    toCode _ cop =
        case (toFnX cop :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnPs name args



instance ToCode PS Blend where
    toCode :: Proxy PS -> Blend -> String
    toCode _ blend =
        case (toFnX blend :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnPs name args



instance ToCode PS Modulate where
    toCode :: Proxy PS -> Modulate -> String
    toCode _ mod =
        case (toFnX mod :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnPs name args



instance ToCode PS Geometry where
    toCode :: Proxy PS -> Geometry -> String
    toCode _ geom =
        case (toFnX geom :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnPs name args


instance ToCode PS SourceN where
    toCode :: Proxy PS -> SourceN -> String
    toCode _ = case _ of
        Source0 -> "s0"


instance ToCode PS OutputN where
    toCode :: Proxy PS -> OutputN -> String
    toCode _ = case _ of
        Output0 -> "o0"
        Output1 -> "o1"
        Output2 -> "o2"
        Output3 -> "o3"
        Output4 -> "o4"


instance ToCode PS Values where
    toCode :: Proxy PS -> Values -> String
    toCode _ (Values array) = "[" <> String.joinWith "," (toCode pureScript <$> array) <> "]" -- FIXME: use `ease`


instance ToCode PS Ease where
    toCode :: Proxy PS -> Ease -> String
    toCode _ ease =
        case (toFnX ease :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnPs name args


instance ToCode PS AudioBin where
    toCode :: Proxy PS -> AudioBin -> String
    toCode _ (AudioBin n) = "H" <> Core.show n


instance ToCode PS AudioSource where
    toCode :: Proxy PS -> AudioSource -> String
    toCode _ = case _ of
        Silence -> "s"
        Mic -> "a"
        File -> "file"


instance ToCode PS TOrV where
    toCode :: Proxy PS -> TOrV -> String
    toCode _ = case _ of
        T tex -> toCode pureScript tex
        V v -> toCode pureScript v


instance ToCode PS Texture where
    toCode :: Proxy PS -> Texture -> String
    toCode _ = case _ of
        Empty -> "{- empty -}"
        Start (Load outputN) -> "(src "  <> toCode pureScript outputN <> ")"
        Start (External srcN _) -> "(src "  <> toCode pureScript srcN <> ")"
        Start src -> toCode pureScript src
        -- FIXME: use PossiblyToFn
        BlendOf { what, with } blend ->
            toCode pureScript with <> "\n\t# " <>
            case (toFnX blend :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnsPs name $ toCode pureScript what : (toCode pureScript <$> args)
        Filter texture cop ->
            toCode pureScript texture <> "\n\t# " <>
            case (toFnX cop :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnPs name args
        ModulateWith { what, with } mod ->
            toCode pureScript with <> "\n\t# " <>
            case (toFnX mod :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnsPs name $ toCode pureScript what : (toCode pureScript <$> args)
        Geometry texture gmt ->
            toCode pureScript texture <> "\n\t# " <>
            case (toFnX gmt :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnPs name args
        CallGlslFn { over, mbWith } fnRef ->
            case over of
                Empty -> ""
                _ -> toCode pureScript over <> "\n\t#"
            <>
            case (toFnX fnRef :: String /\ Array GlslFnArg /\ Array GlslFnOut) of
                name /\ args /\ _ -> fnsPs name $ case mbWith of
                    Just with -> toCode pureScript with : (toCode pureScript <$> args)
                    Nothing -> toCode pureScript <$> args


instance ToCode PS OnAudio where
    toCode :: Proxy PS -> OnAudio -> String
    toCode _ = case _ of
        Show audio -> toCode pureScript audio <> " # " <> fnePs "show"
        Hide audio -> toCode pureScript audio <> " # " <> fnePs "hide"
        SetBins audio n -> toCode pureScript audio <> " # " <> fnsPs "setBins" [ Core.show n ]
        SetCutoff audio n -> toCode pureScript audio <> " # " <> fnsPs "setCutoff" [ Core.show n ]
        SetScale audio n -> toCode pureScript audio <> " # " <> fnsPs "setScale" [ Core.show n ]
        SetSmooth audio n -> toCode pureScript audio <> " # " <> fnsPs "setSmooth" [ Core.show n ]


{- JAVASCRIPT -}


instance ToCode JS Value where
    toCode :: Proxy JS -> Value -> String
    toCode _ = case _ of
        None -> "/* none */"
        Undefined -> "/* ?? */"
        Number n -> Core.show n
        VArray vals ease -> toCode javaScript vals <> "\n\t." <> toCode javaScript ease
        Dep (UserExpr jsExpr) -> "() => " <> toCode javaScript jsExpr
        Dep (Unparsed jsExprStr) -> "() => " <> jsExprStr
        Dep (Fn _) -> "/* dep-fn */"
        Dep NoAction -> "/* nothing */"
        Time -> "time"
        MouseX -> "mouse.x"
        MouseY -> "mouse.y"
        Width -> "width"
        Height -> "height"
        Pi -> "Math.PI"
        Fft bin -> "a.fft[" <> toCode javaScript bin <> "]"
        -- Audio audio bin -> toCode javaScript audio <> ".fft[" <> toCode javaScript bin <> "]"


instance ToCode JS Source where
    toCode :: Proxy JS -> Source -> String
    toCode _ = case _ of
        Load outputN -> toCode javaScript outputN
        External sourceN _ -> toCode javaScript sourceN
        Gradient { speed } -> fnJs "gradient" [ speed ]
        Noise { scale, offset } -> fnJs "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fnJs "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fnJs "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fnJs "solid" [ r, g, b, a ]
        Voronoi { scale, speed, blending } -> fnJs "voronoi" [ scale, speed, blending ]


instance ToCode JS ColorOp where
    toCode :: Proxy JS -> ColorOp -> String
    toCode _ cop =
        case (toFnX cop :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnJs name args


instance ToCode JS Blend where
    toCode :: Proxy JS -> Blend -> String
    toCode _ blend =
        case (toFnX blend :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnJs name args


instance ToCode JS Modulate where
    toCode :: Proxy JS -> Modulate -> String
    toCode _ mod =
        case (toFnX mod :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnJs name args


instance ToCode JS Geometry where
    toCode :: Proxy JS -> Geometry -> String
    toCode _ geom =
        case (toFnX geom :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnJs name args


instance ToCode JS SourceN where
    toCode :: Proxy JS -> SourceN -> String
    toCode _ = case _ of
        Source0 -> "s0"


instance ToCode JS OutputN where
    toCode :: Proxy JS -> OutputN -> String
    toCode _ = case _ of
        Output0 -> "o0"
        Output1 -> "o1"
        Output2 -> "o2"
        Output3 -> "o3"
        Output4 -> "o4"


instance ToCode JS Ease where
    toCode :: Proxy JS -> Ease -> String
    toCode _ ease =
        case (toFnX ease :: String /\ Array Value /\ Array Unit) of
            name /\ args /\ _ -> fnJs name args


instance ToCode JS AudioBin where
    toCode :: Proxy JS -> AudioBin -> String
    toCode _ (AudioBin n) = Core.show n


instance ToCode JS AudioSource where
    toCode :: Proxy JS -> AudioSource -> String
    toCode _ = case _ of
        Silence -> "s"
        Mic -> "a"
        File -> "file"


instance ToCode JS Values where
    toCode :: Proxy JS -> Values -> String
    toCode _ (Values array) = "[" <> String.joinWith "," (toCode javaScript <$> array) <> "]" -- FIXME: use `ease`


instance ToCode JS TOrV where
    toCode :: Proxy JS -> TOrV -> String
    toCode _ = case _ of
        T tex -> toCode javaScript tex
        V v -> toCode javaScript v


instance ToCode JS Texture where
    toCode :: Proxy JS -> Texture -> String
    toCode _ = case _ of
        Empty -> "NaN" -- "/* empty */"
        Start (External sn _) -> "src("  <> toCode javaScript sn <> ")"
        Start (Load out) -> "src("  <> toCode javaScript out <> ")"
        Start src -> toCode javaScript src
        BlendOf { what, with } blend ->
            toCode javaScript what <> "\n\t." <>
            case (toFnX blend :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnsJs name $ toCode javaScript with : (toCode javaScript <$> args)
        Filter texture cop ->
            toCode javaScript texture <> "\n\t." <>
            case (toFnX cop :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnJs name args
        ModulateWith { what, with } mod ->
            toCode javaScript what <> "\n\t." <>
            case (toFnX mod :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnsJs name $ toCode javaScript with : (toCode javaScript <$> args)
        Geometry texture gmt ->
            toCode javaScript texture <> "\n\t." <>
            case (toFnX gmt :: String /\ Array Value /\ Array Unit) of
                name /\ args /\ _ -> fnJs name args
        CallGlslFn { over, mbWith } fnRef ->
            case over of
                Empty -> ""
                _ -> toCode javaScript over <> "\n\t."
            <>
            case (toFnX fnRef :: String /\ Array GlslFnArg /\ Array GlslFnOut) of
                name /\ args /\ _ -> fnsJs name $ case mbWith of
                    Just with -> toCode javaScript with : (toCode javaScript <$> args)
                    Nothing -> toCode javaScript <$> args


instance ToCode JS OnAudio where
    toCode :: Proxy JS -> OnAudio -> String
    toCode _ = case _ of
        Show audio -> toCode javaScript audio <> "." <> fneJs "show"
        Hide audio -> toCode javaScript audio <> "." <> fneJs "hide"
        SetBins audio n -> toCode javaScript audio <> "." <> fnsJs "setBins" [ Core.show n ]
        SetCutoff audio n -> toCode javaScript audio <> "." <> fnsJs "setCutoff" [ Core.show n ]
        SetScale audio n -> toCode javaScript audio <> "." <> fnsJs "setScale" [ Core.show n ]
        SetSmooth audio n -> toCode javaScript audio <> "." <> fnsJs "setSmooth" [ Core.show n ]


instance ToCode lang WrapRepr where
    toCode :: Proxy lang -> WrapRepr -> String
    toCode _ = case _ of
        _ -> "" -- FIXME: implement


instance ToCode JS GlslFn where
    toCode :: Proxy JS -> GlslFn -> String
    toCode _ = case _ of
        GlslFn (kind /\ GlslFnCode code /\ fn) ->
            "setFunction({\n"
                <> "name : \'" <> name fn <> "\',\n"
                <> "type : \'" <> kindToString kind <> "\',\n"
                <> "inputs : [ " <> String.joinWith "," (argToJsObj <$> args fn) <> " ], \n"
                <> "glsl : `" <> code
                <> "`\n});"
        where
            argToJsObj =
                case _ of
                    (name /\ T _) -> "{ type: 'vec4', default : NaN, name : \'" <> name <> "\' }"
                    (name /\ V (Number n)) -> "{ type: 'float', default : " <> Core.show n <> ", name : \'" <> name <> "\' }"
                    (name /\ V val) -> "{ type: 'float', default : " <> toCode javaScript val <> ", name : \'" <> name <> "\' }"
            kindToString = case _ of
                FnSrc -> "src"
                FnCoord -> "coord"
                FnCombineCoord -> "combineCoord"
                FnCombine -> "combine"
                FnColor -> "color"


-- instance ToCode JS a => ToCode JS_DISPLAY a where
--     toCode :: Proxy JS_DISPLAY -> a -> String
--     toCode _ = toCode javaScript


instance ToCode JS_DISPLAY GlslFn where
    toCode :: Proxy JS_DISPLAY -> GlslFn -> String
    toCode _ = case _ of
        GlslFn (kind /\ GlslFnCode code /\ fn) ->
            "setFunction(\'" <> name fn <> "\', " <> kindToString kind <> ",\n"
                <> "[ " <> String.joinWith "," (argToJsObj <$> args fn) <> " ], \n"
                <> "`" <> String.take 40 code <> "..."
                <> "`});"
        where
            argToJsObj =
                case _ of
                    (name /\ T _) -> "{ vec4, \'" <> name <> "\' }"
                    (name /\ V (Number n)) -> "{ float, " <> Core.show n <> ", \'" <> name <> "\' }"
                    (name /\ V val) -> "{ 'float', " <> toCode javaScript val <> ", \'" <> name <> "\' }"
            kindToString = case _ of
                FnSrc -> "src"
                FnCoord -> "coord"
                FnCombineCoord -> "combineCoord"
                FnCombine -> "combine"
                FnColor -> "color"


instance ToCode PS JsExpr where
  toCode :: Proxy PS -> JsExpr -> String
  toCode _ = case _ of
    Val (Number n) -> if n >= 0.0 then Core.show n else "(" <> Core.show n <> ")"
    Val Pi -> "pi"
    Val Time -> "ctx.time"
    (Val (Fft n)) -> "(a # fft h" <> Core.show n <> ")"
    Brackets expr -> "(" <> toCode pureScript expr <> ")"
    Val MouseX -> "ctx.mouseX"
    Val MouseY -> "ctx.mouseY"
    Val Width -> "ctx.width"
    Val Height -> "ctx.height"
    -- Val None -> "undefined"
    Val val -> toCode pureScript val
    DivE a b -> "(" <> toCode pureScript a <> " / " <> toCode pureScript b <> ")"
    MulE a b -> "(" <> toCode pureScript a <> " * " <> toCode pureScript b <> ")"
    AddE a b -> "(" <> toCode pureScript a <> " + " <> toCode pureScript b <> ")"
    SubE a b -> "(" <> toCode pureScript a <> " - " <> toCode pureScript b <> ")"
    ModE a b -> "(" <> toCode pureScript a <> " % " <> toCode pureScript b <> ")"
    Math method (Just expr) -> "(" <> method <> " $ " <> toCode pureScript expr <> ")"
    Math method Nothing -> method


instance ToCode JS JsExpr where
  toCode :: Proxy JS -> JsExpr -> String
  toCode _ = case _ of
    Val (Number n) -> Core.show n
    Val Pi -> "Math.pi"
    Val Time -> "time"
    Val (Fft (AudioBin n)) -> "a.fft[" <> Core.show n <> "]"
    Brackets expr -> "(" <> toCode javaScript expr <> ")"
    Val MouseX -> "mouse.x"
    Val MouseY -> "mouse.y"
    Val Width -> "width"
    Val Height -> "height"
    Val val -> toCode javaScript val
    DivE a b -> toCode javaScript a <> "/" <> toCode javaScript b
    MulE a b -> toCode javaScript a <> "*" <> toCode javaScript b
    AddE a b -> toCode javaScript a <> "+" <> toCode javaScript b
    SubE a b -> toCode javaScript a <> "-" <> toCode javaScript b
    ModE a b -> toCode javaScript a <> "%" <> toCode javaScript b
    Math method (Just expr) -> "Math." <> method <> "(" <> toCode javaScript expr <> ")"
    Math method Nothing -> "Math." <> method <> "()"
