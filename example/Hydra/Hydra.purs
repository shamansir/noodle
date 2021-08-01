module Hydra where


import Prelude (($), (<<<), (>>>), (<$>), flip)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Hydra.Fn (class ToFn, fn)


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
    -- | Source Output


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
    = Add { what :: Entity, amount :: Value }
    | Layer { what :: Entity }
    | Blend { what :: Entity, amount :: Value }
    | Mult { what :: Entity, amount :: Value }
    | Diff { what :: Entity }
    | Mask { what :: Entity }


data Modulate
    = ModulateRepeat { what :: Entity, repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | ModulateRepeatX { what :: Entity, reps :: Value, offset :: Value }
    | ModulateRepeatY { what :: Entity, reps :: Value, offset :: Value }
    | ModulateKaleid { what :: Entity, nSides :: Value }
    | ModulateScrollX { what :: Entity, scrollX :: Value, speed :: Value }
    | ModulateScrollY { what :: Entity, scrollY :: Value, speed :: Value }
    | Modulate { what :: Entity, amount :: Value }
    | ModulateScale { what :: Entity, multiple :: Value, offset :: Value }
    | ModulatePixelate { what :: Entity, multiple :: Value, offset :: Value }
    | ModulateRotate { what :: Entity, multiple :: Value, offset :: Value }
    | ModulateHue { what :: Entity, amount :: Value }


data Modifier = G Geometry | C Color | B Blend | M Modulate


data Entity =
    Entity Source (Array Modifier)


data Output
    = Default
    | Output Int


type Queue =
    Array (Entity /\ Output)


data Hydra
    = None
    | Value Value
    | Hydra Entity
    | Out Queue


default :: Hydra
default = None


entityOf :: Source -> Entity
entityOf src = Entity src []


hydraOf :: Entity -> Hydra
hydraOf = Hydra


num :: Number -> Hydra
num = Value <<< Num


seq :: Array Number -> Hydra
seq = Value <<< Seq


defaultSource :: Source
defaultSource = Osc { freq : Num 60.0, sync : Num 0.1, offset : Num 0.0 }


defaultEntity :: Entity
defaultEntity = entityOf defaultSource


isValue :: Hydra -> Boolean
isValue (Value _) = true
isValue _ = false


isEntity :: Hydra -> Boolean
isEntity (Hydra _) = true
isEntity _ = false


isOut :: Hydra -> Boolean
isOut (Out _) = true
isOut _ = false


type HydraFn1 = Hydra -> Hydra
type HydraFn2 = Hydra -> Hydra -> Hydra
type HydraFn3 = Hydra -> Hydra -> Hydra -> Hydra
type HydraFn4 = Hydra -> Hydra -> Hydra -> Hydra -> Hydra
type HydraFn5 = Hydra -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra


type HydraEFn0 = HydraFn1
type HydraEFn1 = HydraFn2
type HydraEFn2 = HydraFn3
type HydraEFn3 = HydraFn4
type HydraEFn4 = HydraFn5


type ToHydraFn1 = Value -> Hydra
type ToHydraFn2 = Value -> Value -> Hydra
type ToHydraFn3 = Value -> Value -> Value -> Hydra
type ToHydraFn4 = Value -> Value -> Value -> Value -> Hydra
type ToHydraFn5 = Value -> Value -> Value -> Value -> Value -> Hydra


type ToHydraEFn0 = Entity -> Hydra
type ToHydraEFn1 = Entity -> Value -> Hydra
type ToHydraEFn2 = Entity -> Value -> Value -> Hydra
type ToHydraEFn3 = Entity -> Value -> Value -> Value -> Hydra
type ToHydraEFn4 = Entity -> Value -> Value -> Value -> Value -> Hydra


data EntityOrValue
    = E -- entity
    | V -- value


data EntityOrValue'
    = E' Entity
    | V' Value


v1 :: String -> Array (String /\ EntityOrValue)
v1 v1 = [ v1 /\ V ]


v2 :: String -> String -> Array (String /\ EntityOrValue)
v2 v1 v2 = flip (/\) V <$> [ v1, v2 ]


v3 :: String -> String -> String -> Array (String /\ EntityOrValue)
v3 v1 v2 v3 = flip (/\) V <$> [ v1, v2, v3 ]


v4 :: String -> String -> String -> String -> Array (String /\ EntityOrValue)
v4 v1 v2 v3 v4 = flip (/\) V <$> [ v1, v2, v3, v4 ]


v5 :: String -> String -> String -> String -> String -> Array (String /\ EntityOrValue)
v5 v1 v2 v3 v4 v5 = flip (/\) V <$> [ v1, v2, v3, v4, v5 ]


e :: String -> String /\ EntityOrValue
e = flip (/\) E


instance ToFn Source EntityOrValue where
    toFn (Noise _)    = fn "noise" (v2 "scale" "offset") -- $ e "noise"
    toFn (Voronoi _)  = fn "voronoi" (v3 "scale" "speed" "blending") -- $ e "voronoi"
    toFn (Osc _)      = fn "osc" (v3 "freq" "sync" "offset") -- $ e "osc"
    toFn (Shape _)    = fn "shape" (v3 "sides" "radius" "smoothing") -- $ e "shape"
    toFn (Gradient _) = fn "gradient" (v1 "speed") -- $ e "gradient"
    toFn (Solid _)    = fn "solid" (v4 "r" "g" "b" "a") -- $ e "solid"


instance ToFn Source Value where
    toFn (Noise vs)    = fn "noise" [ "scale" /\ vs.scale, "offset" /\ vs.offset ]
    toFn (Voronoi vs)  = fn "voronoi" [ "scale" /\ vs.scale, "speed" /\ vs.speed, "blending" /\ vs.blending ]
    toFn (Osc vs)      = fn "osc" [ "freq" /\ vs.freq, "sync" /\ vs.sync, "offset" /\ vs.offset ]
    toFn (Shape vs)    = fn "shape" [ "sides" /\ vs.sides, "radius" /\ vs.radius, "smoothing" /\ vs.smoothing ]
    toFn (Gradient vs) = fn "gradient" [ "speed" /\ vs.speed ]
    toFn (Solid vs)    = fn "solid" [ "r" /\ vs.r, "g" /\ vs.g, "b" /\ vs.b, "a" /\ vs.a ]