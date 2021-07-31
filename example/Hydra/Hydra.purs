module Hydra where


import Prelude (($), (<<<))
import Data.Maybe (Maybe(..))


type Fn = { name :: String, args :: Array String, out :: String }


class ToFn a where
    toFn :: a -> Fn


data Value
    = Num Number
    | Mouse
    | Time
    | Seq (Array Number)
    -- Harmonic Int
    -- FN (Time -> Value)

data Source
    = Noise { scale :: Value, offset :: Value }
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }
    | Osc { freq :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Gradient { speed :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }


data Geometry
    = Rotate { angle :: Value, speed :: Value }
    | Scale { amount :: Value, xMult :: Value, yMult :: Value, offsetX :: Value, offsetY :: Value }
    | Pixelate { pixelX :: Value, pixelY :: Value }
    | Repeat { repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | RepeatX { reps :: Value, offset :: Value }
    | RepeatY { reps :: Value, offset :: Value }
    | Kaleid { nSides :: Value }
    | ScrollX { scrollX :: Value, speed :: Value }
    | ScrollY { scrollY :: Value, speed :: Value }


data Color
    = Posterize { bins :: Value, gamma :: Value }
    | Shift { r :: Value, g :: Value, b :: Value, a :: Value }
    | Invert { amount :: Value }
    | Contrast { amount :: Value }
    | Brightness { amount :: Value }
    | Luma { treshold :: Value, tolerance :: Value }
    | Tresh { treshold :: Value, tolerance :: Value }
    | Color { r :: Value, g :: Value, b :: Value, a :: Value }
    | Saturate { amount :: Value }
    | Hue { amount :: Value }
    | Colorama { amount :: Value }


data Blend
    = Add { what :: Source, amount :: Value }
    | Layer { what :: Source }
    | Blend { what :: Source, amount :: Value }
    | Mult { what :: Source, amount :: Value }
    | Diff { what :: Source }
    | Mask { what :: Source }


data Modulate
    = ModulateRepeat { what :: Source, repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | ModulateRepeatX { what :: Source, reps :: Value, offset :: Value }
    | ModulateRepeatY { what :: Source, reps :: Value, offset :: Value }
    | ModulateKaleid { what :: Source, nSides :: Value }
    | ModulateScrollX { what :: Source, scrollX :: Value, speed :: Value }
    | ModulateScrollY { what :: Source, scrollY :: Value, speed :: Value }
    | Modulate { what :: Source, amount :: Value }
    | ModulateScale { what :: Source, multiple :: Value, offset :: Value }
    | ModulatePixelate { what :: Source, multiple :: Value, offset :: Value }
    | ModulateRotate { what :: Source, multiple :: Value, offset :: Value }
    | ModulateHue { what :: Source, amount :: Value }


data Hydra
    = None
    | Hydra Source
        (Array Geometry)
        (Array Color)
        (Array Blend)
        (Array Modulate)
    | Out (Maybe Int) Hydra


default :: Hydra
default = None


defaultOsc :: Hydra
defaultOsc = osc 60.0 0.1 0.0


osc :: Number -> Number -> Number -> Hydra
osc frequency syn offset =
    Osc (Num frequency) (Num syn) (Num offset)


tryOsc :: Hydra -> Hydra -> Hydra -> Hydra
tryOsc (Value' freq) (Value' sync) (Value' offset) =
    Osc freq sync offset
tryOsc _ _ _ = None


num :: Number -> Hydra
num = Value' <<< Num


numOr :: Number -> Hydra -> Number
numOr _ (Value' (Num n)) = n
numOr def _ = def


seq :: Array Number -> Hydra
seq = Value' <<< Seq


seq' :: Hydra -> Array Number
seq' (Value' (Seq s)) = s
seq' _ = []


out :: Int -> Hydra -> Hydra
out = Out <<< Just


out' :: Hydra -> Hydra
out' = Out $ Nothing
