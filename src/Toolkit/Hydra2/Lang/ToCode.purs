module Toolkit.Hydra2.Lang.ToCode where

import Prelude
import Prelude (show) as Core

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Semigroup ((<>))
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Toolkit.Hydra2.Types

class ToCode a where
    toCode :: a -> String


class ToFn arg a where
    toFn :: a -> String /\ Array arg


-- fn :: String -> Array (String /\ Value) -> String
-- fn name vals = name <> "(" <> (String.joinWith "," $ toCode <$> Tuple.snd <$> vals) <> ")"


fn :: forall a. ToCode a => String -> Array a -> String
fn name vals = fns name $ toCode <$> vals


fns :: String -> Array String -> String
fns name vals = name <> " ( " <> (String.joinWith " " vals) <> " ) "


fne :: String -> String
fne name = name <> " (  ) "


instance ToCode Value where
    toCode :: Value -> String
    toCode = case _ of
        None -> "/* none */"
        Required -> "/* required */"
        Number n -> Core.show n
        VArray vals ease -> toCode vals -- FIXME: use `ease`
        Dep _ -> "<Dep>"
        Time -> "time"
        MouseX -> "mouseX"
        MouseY -> "mouseY"
        Width -> "width"
        Height -> "height"
        Pi -> "pi"
        Audio audio bin -> "a.fft[" <> toCode bin <> "]" -- FIXME: use `audio`


instance ToCode Source where
    toCode :: Source -> String
    toCode = case _ of
        Dynamic -> "/* dyn */"
        Video -> "/* video */"
        S0 -> "s0"
        Gradient { speed } -> fn "gradient" [ speed ]
        Camera -> "/* camera */" -- 🎥
        Noise { scale, offset } -> fn "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fn "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fn "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fn "solid" [ r, g, b, a ]
        Source from -> toCode from <> "()"
        Voronoi { scale, speed, blending } -> fn "voronoi" [ scale, speed, blending ]


instance ToFn Value ColorOp where
    toFn :: ColorOp -> String /\ Array Value
    toFn = case _ of
        R { scale, offset } -> "r" /\ [ scale, offset ]
        G { scale, offset } -> "g" /\ [ scale, offset ]
        B { scale, offset } -> "b" /\ [ scale, offset ]
        A { scale, offset } -> "a" /\ [ scale, offset ]
        Posterize { bins, gamma } -> "a" /\ [ bins, gamma ]
        Shift { r, g, b, a } -> "shift" /\ [ r, g, b, a ]
        Invert amount -> "invert" /\ [ amount ]
        Contrast amount -> "contrast" /\ [ amount ]
        Brightness amount -> "brightness" /\ [ amount ]
        Luma { treshold, tolerance } -> "luma" /\ [ treshold, tolerance ]
        Tresh { treshold, tolerance } -> "tresh" /\ [ treshold, tolerance ]
        Color { r, g, b, a } -> "color" /\ [ r, g, b, a ]
        Saturate amount -> "saturate" /\ [ amount ]
        Hue amount -> "hue" /\ [ amount ]
        Colorama amount -> "colorama" /\ [ amount ]


instance ToFn Value Modulate where
    toFn :: Modulate -> String /\ Array Value
    toFn = case _ of
        Modulate amount -> "modulate" /\ [ amount ]
        ModHue amount -> "modHue" /\ [ amount ]
        ModKaleid { nSides } -> "modKaleid" /\ [ nSides ]
        ModPixelate { multiple, offset } -> "modPixelate" /\ [ multiple, offset ]
        ModRepeat { offsetX, offsetY, repeatX, repeatY } -> "modRepeat" /\ [ offsetX, offsetY, repeatX, repeatY ]
        ModRepeatX { offset, reps } -> "modRepeatX" /\ [ offset, reps ]
        ModRepeatY { offset, reps } -> "modRepeatY" /\ [ offset, reps ]
        ModRotate { multiple, offset } -> "modRotate" /\ [ multiple, offset ]
        ModScale { multiple, offset } -> "modScale" /\ [ multiple, offset ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ scrollX, scrollY, speedX, speedY ]
        ModScrollX { scrollX, speed } -> "modScrollX" /\ [ scrollX, speed ]
        ModScrollY { scrollY, speed } -> "modScrollY" /\ [ scrollY, speed ]


instance ToFn Value Blend where
    toFn :: Blend -> String /\ Array Value
    toFn = case _ of
        Blend amount -> "blend" /\ [ amount ]
        Add amount -> "add" /\ [ amount ]
        Sub amount -> "sub" /\ [ amount ]
        Mult amount -> "mult" /\ [ amount ]
        Diff -> "diff" /\ []
        Layer _ -> "layer" /\ []
        Mask -> "mask" /\ []


instance ToFn Value Geometry where
    toFn :: Geometry -> String /\ Array Value
    toFn = case _ of
        GKaleid { nSides } -> "kaleid" /\ [ nSides ]
        GPixelate { pixelX, pixelY } -> "pixelate" /\ [ pixelX, pixelY ]
        GRepeat { offsetX, offsetY, repeatX, repeatY } -> "repeat" /\ [ offsetX, offsetY, repeatX, repeatY ]
        GRepeatX { offset, reps } -> "repeatX" /\ [ offset, reps ]
        GRepeatY { offset, reps } -> "repeatY" /\ [ offset, reps ]
        GRotate { angle, speed } -> "rotate" /\ [ angle, speed ]
        GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ amount, xMult, yMult, offsetX, offsetY ]
        GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ scrollX, scrollY, speedX, speedY ]
        GScrollX { scrollX, speed } -> "scrollX" /\ [ scrollX, speed ]
        GScrollY { scrollY, speed } -> "scrollY" /\ [ scrollY, speed ]


instance ToCode ColorOp where
    toCode :: ColorOp -> String
    toCode cop =
        case (toFn cop :: String /\ Array Value) of
            name /\ args -> fn name args


instance ToCode Blend where
    toCode :: Blend -> String
    toCode blend =
        case (toFn blend :: String /\ Array Value) of
            name /\ args -> fn name args


instance ToCode Modulate where
    toCode :: Modulate -> String
    toCode mod =
        case (toFn mod :: String /\ Array Value) of
            name /\ args -> fn name args


instance ToCode Geometry where
    toCode :: Geometry -> String
    toCode geom =
        case (toFn geom :: String /\ Array Value) of
            name /\ args -> fn name args


instance ToCode From where
    toCode :: From -> String
    toCode = case _ of
        All -> "/* all */"
        Output out -> toCode out


instance ToCode Output where
    toCode :: Output -> String
    toCode = case _ of
        Screen -> "/* screen */"
        Output0 -> "o0"
        Output1 -> "o1"
        Output2 -> "o2"


instance ToCode Values where
    toCode :: Values -> String
    toCode (Values array) = "[" <> String.joinWith "," (toCode <$> array) <> "]" -- FIXME: use `ease`


instance ToCode Ease where
    toCode :: Ease -> String
    toCode = case _ of
        Linear -> "linear"
        Fast v -> fn "fast" [ v ]
        Smooth v -> fn "smooth" [ v ]
        Fit { low, high } -> fn "fit" [ low, high ]
        Offset v -> fn "offset" [ v ]
        InOutCubic -> "inOutCubic"


instance ToCode AudioBin where
    toCode :: AudioBin -> String
    toCode = case _ of
        H0 -> "0"
        H1 -> "1"
        H2 -> "2"
        H3 -> "3"
        H4 -> "4"


instance ToCode Texture where
    toCode :: Texture -> String
    toCode = case _ of
        Empty -> "/* empty */"
        From src -> toCode src
        BlendOf { what, with } blend ->
            toCode with <> " # " <>
            case (toFn blend :: String /\ Array Value) of
                name /\ args -> fns name $ toCode what : (toCode <$> args)
        WithColor texture cop ->
            toCode texture <> " # " <>
            case (toFn cop :: String /\ Array Value) of
                name /\ args -> fn name args
        ModulateWith { what, with } mod ->
            toCode with <> " # " <>
            case (toFn mod :: String /\ Array Value) of
                name /\ args -> fns name $ toCode what : (toCode <$> args)
        Geometry texture gmt ->
            toCode texture <> " # " <>
            case (toFn gmt :: String /\ Array Value) of
                name /\ args -> fn name args