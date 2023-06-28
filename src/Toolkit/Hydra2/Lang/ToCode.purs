module Toolkit.Hydra2.Lang.ToCode where

import Prelude
import Prelude (show) as Core

import Data.String as String
import Data.Semigroup ((<>))
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Toolkit.Hydra2.Types

class ToCode a where
    toCode :: a -> String


-- fn :: String -> Array (String /\ Value) -> String
-- fn name vals = name <> "(" <> (String.joinWith "," $ toCode <$> Tuple.snd <$> vals) <> ")"


fn :: String -> Array Value -> String
fn name vals = name <> "(" <> (String.joinWith "," $ toCode <$> vals) <> ")"


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
        Gradient { speed } -> fn "gradient" [ speed ]
        Camera -> "/* camera */" -- 🎥
        Noise { scale, offset } -> fn "noise" [ scale, offset ]
        Osc { frequency, sync, offset } -> fn "osc" [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> fn "shape" [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> fn "solid" [ r, g, b, a ]
        Source from -> toCode from <> "()"
        Voronoi { scale, speed, blending } -> fn "voronoi" [ scale, speed, blending ]


instance ToCode ColorOp where
    toCode :: ColorOp -> String
    toCode = case _ of
        R { scale, offset } -> fn "r" [ scale, offset ]
        G { scale, offset } -> fn "g" [ scale, offset ]
        B { scale, offset } -> fn "b" [ scale, offset ]
        A { scale, offset } -> fn "a" [ scale, offset ]
        Posterize { bins, gamma } -> fn "a" [ bins, gamma ]
        Shift { r, g, b, a } -> fn "shift" [ r, g, b, a ]
        Invert amount -> fn "invert" [ amount ]
        Contrast amount -> fn "contrast" [ amount ]
        Brightness amount -> fn "brightness" [ amount ]
        Luma { treshold, tolerance } -> fn "luma" [ treshold, tolerance ]
        Tresh { treshold, tolerance } -> fn "tresh" [ treshold, tolerance ]
        Color { r, g, b, a } -> fn "color" [ r, g, b, a ]
        Saturate amount -> fn "saturate" [ amount ]
        Hue amount -> fn "hue" [ amount ]
        Colorama amount -> fn "colorama" [ amount ]


instance ToCode Modulate where
    toCode :: Modulate -> String
    toCode = case _ of
        Modulate amount -> fn "modulate" [ amount ]
        ModHue amount -> fn "modHue" [ amount ]
        ModKaleid { nSides } -> fn "modKaleid" [ nSides ]
        ModPixelate { multiple, offset } -> fn "modPixelate" [ multiple, offset ]
        ModRepeat { offsetX, offsetY, repeatX, repeatY } -> fn "modRepeat" [ offsetX, offsetY, repeatX, repeatY ]
        ModRepeatX { offset, reps } -> fn "modRepeatX" [ offset, reps ]
        ModRepeatY { offset, reps } -> fn "modRepeatY" [ offset, reps ]
        ModRotate { multiple, offset } -> fn "modRotate" [ multiple, offset ]
        ModScale { multiple, offset } -> fn "modScale" [ multiple, offset ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> fn "modScroll" [ scrollX, scrollY, speedX, speedY ]
        ModScrollX { scrollX, speed } -> fn "modScrollX" [ scrollX, speed ]
        ModScrollY { scrollY, speed } -> fn "modScrollY" [ scrollY, speed ]


instance ToCode Geometry where
    toCode :: Geometry -> String
    toCode = case _ of
        GKaleid { nSides } -> fn "kaleid" [ nSides ]
        GPixelate { pixelX, pixelY } -> fn "pixelate" [ pixelX, pixelY ]
        GRepeat { offsetX, offsetY, repeatX, repeatY } -> fn "repeat" [ offsetX, offsetY, repeatX, repeatY ]
        GRepeatX { offset, reps } -> fn "repeatX" [ offset, reps ]
        GRepeatY { offset, reps } -> fn "repeatY" [ offset, reps ]
        GRotate { angle, speed } -> fn "rotate" [ angle, speed ]
        GScale { amount, xMult, yMult, offsetX, offsetY } -> fn "scale" [ amount, xMult, yMult, offsetX, offsetY ]
        GScroll { scrollX, scrollY, speedX, speedY } -> fn "scroll" [ scrollX, scrollY, speedX, speedY ]
        GScrollX { scrollX, speed } -> fn "scrollX" [ scrollX, speed ]
        GScrollY { scrollY, speed } -> fn "scrollY" [ scrollY, speed ]


instance ToCode From where
    toCode :: From -> String
    toCode = case _ of
        All -> "/* all *."
        S0 -> "s0"
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





{- TODO:
instance ToCode Texture where
    toCode :: Texture -> String
    toCode = case _ of
        Empty -> "/* empty */"
        From src -> toCode src <> "()"
        BlendOf { what, with } blend -> fn "blend" [ what, with ]
        WithColor texture op -> "With Color " <> {- TODO : show op <> " " <> -|} show texture
        ModulateWith { what, with } mod -> "Modulate " <> show what <> " -< " <> show with {- TODO: <> " " <> show mod -|}
        Geometry texture gmt -> "Geometry " <> show texture {- TODO: <> " " <> show gmt -|}
-}