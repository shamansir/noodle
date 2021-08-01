module Hydra where


import Prelude ((<$>), (<<<))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))

import Hydra.Fn (class ToFn, fn, toFn)


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
    Array (Entity /\ Output) -- TODO: non-empty array


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


type EntityOrValue = Either Entity Value


e' :: Entity -> EntityOrValue
e' = Left


v' :: Value -> EntityOrValue
v' = Right


instance ToFn Source Value where
    toFn (Noise vs)    = fn "noise" [ "scale" /\ vs.scale, "offset" /\ vs.offset ]
    toFn (Voronoi vs)  = fn "voronoi" [ "scale" /\ vs.scale, "speed" /\ vs.speed, "blending" /\ vs.blending ]
    toFn (Osc vs)      = fn "osc" [ "freq" /\ vs.freq, "sync" /\ vs.sync, "offset" /\ vs.offset ]
    toFn (Shape vs)    = fn "shape" [ "sides" /\ vs.sides, "radius" /\ vs.radius, "smoothing" /\ vs.smoothing ]
    toFn (Gradient vs) = fn "gradient" [ "speed" /\ vs.speed ]
    toFn (Solid vs)    = fn "solid" [ "r" /\ vs."r", "g" /\ vs.g, "b" /\ vs.b, "a" /\ vs.a ]


instance ToFn Geometry Value where
    toFn (Rotate vs)   = fn "rotate" [ "angle" /\ vs.angle, "speed" /\ vs.speed ]
    toFn (Scale vs)    = fn "scale"
                            [ "amount" /\ vs.amount
                            , "xMult" /\ vs.xMult, "yMult" /\ vs.yMult
                            , "offsetX" /\ vs.offsetX, "offsetY" /\ vs.offsetY
                            ]
    toFn (Pixelate vs) = fn "pixelate" [ "pixelX" /\ vs.pixelX, "pixelY" /\ vs.pixelY ]
    toFn (Repeat vs)   = fn "repeat"
                            [ "repeatX" /\ vs.repeatX, "repeatY" /\ vs.repeatY
                            , "offsetX" /\ vs.offsetX, "offsetY" /\ vs.offsetY
                            ]
    toFn (RepeatX vs) = fn "repeatX" [ "reps" /\ vs.reps, "offset" /\ vs.offset ]
    toFn (RepeatY vs) = fn "repeatY" [ "reps" /\ vs.reps, "offset" /\ vs.offset ]
    toFn (Kaleid vs)  = fn "kaleid" [ "nSides" /\ vs.nSides ]
    toFn (ScrollX vs) = fn "scrollX" [ "scrollX" /\ vs.scrollX, "speed" /\ vs.speed ]
    toFn (ScrollY vs) = fn "scrollY" [ "scrollY" /\ vs.scrollY, "speed" /\ vs.speed ]


instance ToFn Color Value where
    toFn (Posterize vs)  = fn "posterize" [ "bins" /\ vs.bins, "gamma" /\ vs.gamma ]
    toFn (Shift vs)      = fn "shift" [ "r" /\ vs."r", "g" /\ vs.g, "b" /\ vs.b, "a" /\ vs.a ]
    toFn (Invert vs)     = fn "invert" [ "amount" /\ vs.amount ]
    toFn (Contrast vs)   = fn "contrast" [ "amount" /\ vs.amount ]
    toFn (Brightness vs) = fn "brightness" [ "amount" /\ vs.amount ]
    toFn (Luma vs)       = fn "luma" [ "treshold" /\ vs.treshold, "tolerance" /\ vs.tolerance ]
    toFn (Tresh vs)      = fn "tresh" [ "treshold" /\ vs.treshold, "tolerance" /\ vs.tolerance ]
    toFn (Color vs)      = fn "color" [ "r" /\ vs."r", "g" /\ vs.g, "b" /\ vs.b, "a" /\ vs.a ]
    toFn (Saturate vs)   = fn "saturate" [ "amount" /\ vs.amount ]
    toFn (Hue vs)        = fn "hue" [ "amount" /\ vs.amount ]
    toFn (Colorama vs)   = fn "colorama" [ "amount" /\ vs.amount ]


instance ToFn Blend EntityOrValue where
    toFn (Add vs)   = fn "add" [ "what" /\ e' vs.what, "amount" /\ v' vs.amount ]
    toFn (Layer vs) = fn "layer" [ "what" /\ e' vs.what ]
    toFn (Blend vs) = fn "blend" [ "what" /\ e' vs.what, "amount" /\ v' vs.amount ]
    toFn (Mult vs)  = fn "mult" [ "what" /\ e' vs.what, "amount" /\ v' vs.amount ]
    toFn (Diff vs)  = fn "diff" [ "what" /\ e' vs.what ]
    toFn (Mask vs)  = fn "mask" [ "what" /\ e' vs.what ]


instance ToFn Modulate EntityOrValue where
    toFn (ModulateRepeat vs)   = fn "modulateRepeat"
                                    [ "what" /\ e' vs.what
                                    , "repeatX" /\ v' vs.repeatX, "repeatY" /\ v' vs.repeatY
                                    , "offsetX" /\ v' vs.offsetX, "offsetY" /\ v' vs.offsetY
                                    ]
    toFn (ModulateRepeatX vs)  = fn "modulateRepeatX"
                                    [ "what" /\ e' vs.what, "reps" /\ v' vs.reps, "offset" /\ v' vs.offset ]
    toFn (ModulateRepeatY vs)  = fn "modulateRepeatY"
                                    [ "what" /\ e' vs.what, "reps" /\ v' vs.reps, "offset" /\ v' vs.offset ]
    toFn (ModulateKaleid vs)   = fn "modulateKaleid"
                                    [ "what" /\ e' vs.what, "nSides" /\ v' vs.nSides ]
    toFn (ModulateScrollX vs)  = fn "modulateScrollX"
                                    [ "what" /\ e' vs.what, "scrollX" /\ v' vs.scrollX, "speed" /\ v' vs.speed ]
    toFn (ModulateScrollY vs)  = fn "modulateScrollY"
                                    [ "what" /\ e' vs.what, "scrollY" /\ v' vs.scrollY, "speed" /\ v' vs.speed ]
    toFn (Modulate vs)         = fn "modulate" [ "what" /\ e' vs.what, "amount" /\ v' vs.amount ]
    toFn (ModulateScale vs)    = fn "modulateScale"
                                    [ "what" /\ e' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulatePixelate vs) = fn "modulatePixelate"
                                    [ "what" /\ e' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulateRotate vs)   = fn "modulateRotate"
                                    [ "what" /\ e' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulateHue vs)      = fn "modulateHue" [ "what" /\ e' vs.what, "amount" /\ v' vs.amount ]


instance ToFn Modifier EntityOrValue where
    toFn (G geom) = v' <$> toFn geom
    toFn (C color) = v' <$> toFn color
    toFn (B blend) = toFn blend
    toFn (M modulate) = toFn modulate