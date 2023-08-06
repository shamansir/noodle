module Toolkit.Hydra2.Lang.ToCode where

import Prelude (($), (<$>), (<>))
import Prelude (show) as Core

import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap (WrapRepr)
import Toolkit.Hydra2.Lang.Fn (toFnX)

import Data.Array ((:))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))


import Blessed.Tagger as T


data Target


-- could they be split into different files?
-- TODO: NDF format is not bound to Hydra, rather to Noodle Engine, move it to external module
foreign import data JS :: Target
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
        Dynamic -> "{- dyn -}"
        Video -> "{- video -}"
        S0 -> "s0"
        -- FIXME: use PossiblyToFn
        Gradient { speed } -> fnPs "gradient" [ speed ]
        Camera -> "{- camera -}" -- 🎥
        Noise { scale, offset } -> fnPs "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fnPs "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fnPs "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fnPs "solid" [ r, g, b, a ]
        Source from -> toCode pureScript from <> "()"
        Voronoi { scale, speed, blending } -> fnPs "voronoi" [ scale, speed, blending ]



instance ToCode PS ColorOp where
    toCode :: Proxy PS -> ColorOp -> String
    toCode _ cop =
        case (toFnX cop :: String /\ Array Value) of
            name /\ args -> fnPs name args



instance ToCode PS Blend where
    toCode :: Proxy PS -> Blend -> String
    toCode _ blend =
        case (toFnX blend :: String /\ Array Value) of
            name /\ args -> fnPs name args



instance ToCode PS Modulate where
    toCode :: Proxy PS -> Modulate -> String
    toCode _ mod =
        case (toFnX mod :: String /\ Array Value) of
            name /\ args -> fnPs name args



instance ToCode PS Geometry where
    toCode :: Proxy PS -> Geometry -> String
    toCode _ geom =
        case (toFnX geom :: String /\ Array Value) of
            name /\ args -> fnPs name args


instance ToCode PS From where
    toCode :: Proxy PS -> From -> String
    toCode _ = case _ of
        All -> "{- all -}"
        Output out -> toCode pureScript out


instance ToCode PS Output where
    toCode :: Proxy PS -> Output -> String
    toCode _ = case _ of
        Screen -> "{- screen -}"
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
        case (toFnX ease :: String /\ Array Value) of
            name /\ args -> fnPs name args


instance ToCode PS AudioBin where
    toCode :: Proxy PS -> AudioBin -> String
    toCode _ (AudioBin n) = "H" <> Core.show n


instance ToCode PS AudioSource where
    toCode :: Proxy PS -> AudioSource -> String
    toCode _ = case _ of
        Silence -> "s"
        Mic -> "a"
        File -> "file"


instance ToCode PS Texture where
    toCode :: Proxy PS -> Texture -> String
    toCode _ = case _ of
        Empty -> "{- empty -}"
        From S0 -> "(src "  <> toCode pureScript S0 <> ")"
        From src -> toCode pureScript src
        -- FIXME: use PossiblyToFn
        BlendOf { what, with } blend ->
            toCode pureScript with <> "\n\t# " <>
            case (toFnX blend :: String /\ Array Value) of
                name /\ args -> fnsPs name $ toCode pureScript what : (toCode pureScript <$> args)
        WithColor texture cop ->
            toCode pureScript texture <> "\n\t# " <>
            case (toFnX cop :: String /\ Array Value) of
                name /\ args -> fnPs name args
        ModulateWith { what, with } mod ->
            toCode pureScript with <> "\n\t# " <>
            case (toFnX mod :: String /\ Array Value) of
                name /\ args -> fnsPs name $ toCode pureScript what : (toCode pureScript <$> args)
        Geometry texture gmt ->
            toCode pureScript texture <> "\n\t# " <>
            case (toFnX gmt :: String /\ Array Value) of
                name /\ args -> fnPs name args


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
        Dep _ -> "/* dep-fn */"
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
        Dynamic -> "/* dyn */"
        Video -> "/* video */"
        S0 -> "s0"
        Gradient { speed } -> fnJs "gradient" [ speed ]
        Camera -> "/* camera */" -- 🎥
        Noise { scale, offset } -> fnJs "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fnJs "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fnJs "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fnJs "solid" [ r, g, b, a ]
        Source from -> toCode javaScript from <> "()"
        Voronoi { scale, speed, blending } -> fnJs "voronoi" [ scale, speed, blending ]


instance ToCode JS ColorOp where
    toCode :: Proxy JS -> ColorOp -> String
    toCode _ cop =
        case (toFnX cop :: String /\ Array Value) of
            name /\ args -> fnJs name args


instance ToCode JS Blend where
    toCode :: Proxy JS -> Blend -> String
    toCode _ blend =
        case (toFnX blend :: String /\ Array Value) of
            name /\ args -> fnJs name args


instance ToCode JS Modulate where
    toCode :: Proxy JS -> Modulate -> String
    toCode _ mod =
        case (toFnX mod :: String /\ Array Value) of
            name /\ args -> fnJs name args


instance ToCode JS Geometry where
    toCode :: Proxy JS -> Geometry -> String
    toCode _ geom =
        case (toFnX geom :: String /\ Array Value) of
            name /\ args -> fnJs name args


instance ToCode JS From where
    toCode :: Proxy JS -> From -> String
    toCode _ = case _ of
        All -> "/* all */"
        Output out -> toCode javaScript out


instance ToCode JS Output where
    toCode :: Proxy JS -> Output -> String
    toCode _ = case _ of
        Screen -> "/* screen */"
        Output0 -> "o0"
        Output1 -> "o1"
        Output2 -> "o2"
        Output3 -> "o3"
        Output4 -> "o4"


instance ToCode JS Ease where
    toCode :: Proxy JS -> Ease -> String
    toCode _ ease =
        case (toFnX ease :: String /\ Array Value) of
            name /\ args -> fnJs name args


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


instance ToCode JS Texture where
    toCode :: Proxy JS -> Texture -> String
    toCode _ = case _ of
        Empty -> "/* empty */"
        From S0 -> "src("  <> toCode javaScript S0 <> ")"
        From src -> toCode javaScript src
        BlendOf { what, with } blend ->
            toCode javaScript with <> "\n\t." <>
            case (toFnX blend :: String /\ Array Value) of
                name /\ args -> fnsJs name $ toCode javaScript what : (toCode javaScript <$> args)
        WithColor texture cop ->
            toCode javaScript texture <> "\n\t." <>
            case (toFnX cop :: String /\ Array Value) of
                name /\ args -> fnJs name args
        ModulateWith { what, with } mod ->
            toCode javaScript with <> "\n\t." <>
            case (toFnX mod :: String /\ Array Value) of
                name /\ args -> fnsJs name $ toCode javaScript what : (toCode javaScript <$> args)
        Geometry texture gmt ->
            toCode javaScript texture <> "\n\t." <>
            case (toFnX gmt :: String /\ Array Value) of
                name /\ args -> fnJs name args


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