module Hydra where


import Prelude

import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)

import Hydra.Fn (class ToFn, Fn, fn, toFn)


data Buffer
    = Default
    | O0
    | O1
    | O2
    | O3
    | S0
    | S1
    | S2
    | S3


derive instance eqBuffer :: Eq Buffer
derive instance ordBuffer :: Ord Buffer


data Operation
    = Addition
    | Division
    | Multiplication
    | Subtraction


derive instance eqOperation :: Eq Operation


data Value
    = Num Number
    | MouseX
    | MouseY
    | Time
    | Seq (Array Value)
    | CanvasWidth
    | CanvasHeight
    | WindowWidth
    | WindowHeight
    | Pi
    | Expr Value Operation Value
    | Dynamic Value -- () -> Value
    | OfTime Value -- (time) -> Value
    | Harmonic Int
    | Fast Number Value -- TODO: allow only sequences
    | Sqrt Value -- TODO: allow only numbers


{- data Expr
    = V Value
    | Fn (Time -> Value)
    | Op Value Value -}


data Source
    = Noise { scale :: Value, offset :: Value }
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }
    | Osc { freq :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Gradient { speed :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }
    | Source Buffer


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
    = Add { what :: Texture, amount :: Value }
    | Layer { what :: Texture }
    | Blend { what :: Texture, amount :: Value }
    | Mult { what :: Texture, amount :: Value }
    | Diff { what :: Texture }
    | Mask { what :: Texture, reps :: Value, offset :: Value }


data Modulate
    = ModulateRepeat { what :: Texture, repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | ModulateRepeatX { what :: Texture, reps :: Value, offset :: Value }
    | ModulateRepeatY { what :: Texture, reps :: Value, offset :: Value }
    | ModulateKaleid { what :: Texture, nSides :: Value }
    | ModulateScrollX { what :: Texture, scrollX :: Value, speed :: Value }
    | ModulateScrollY { what :: Texture, scrollY :: Value, speed :: Value }
    | Modulate { what :: Texture, amount :: Value }
    | ModulateScale { what :: Texture, multiple :: Value, offset :: Value }
    | ModulatePixelate { what :: Texture, multiple :: Value, offset :: Value }
    | ModulateRotate { what :: Texture, multiple :: Value, offset :: Value }
    | ModulateHue { what :: Texture, amount :: Value }


data Modifier = G Geometry | C Color | B Blend | M Modulate


data Texture
    = Texture Source (Array Modifier)


{- data Buffered
    = Buffered Buffer Texture -}


data Hydra
    = None
    | Op  Operation
    | Val Value
    | Mod Modifier
    | Tex Texture
    | Buf Buffer


-- FIXME: use API for everything below

default :: Hydra
default = None


textureOf :: Source -> Texture
textureOf src = Texture src []


geometry :: Geometry -> Modifier
geometry = G


color :: Color -> Modifier
color = C


blend :: Blend -> Modifier
blend = B


modulate :: Modulate -> Modifier
modulate = M


addModifier :: Texture -> Modifier -> Texture
addModifier (Texture src modifiers) = Texture src <<< Array.snoc modifiers


withModifiers :: Texture -> Array Modifier -> Texture
withModifiers = foldl addModifier


hydraOf :: Texture -> Hydra
hydraOf = Tex


justModifier :: Modifier -> Hydra
justModifier = Mod


buffer :: Buffer -> Hydra
buffer = Buf


num :: Number -> Hydra
num = Val <<< Num


pi :: Hydra
pi = Val Pi


time :: Hydra
time = Val Time


mouseX :: Hydra
mouseX = Val MouseX


mouseY :: Hydra
mouseY = Val MouseY


width :: Hydra
width = Val CanvasWidth


height :: Hydra
height = Val CanvasHeight


seq :: Array Value -> Hydra
seq = Val <<< Seq


sqrt :: Value -> Hydra
sqrt = Val <<< Sqrt


defaultSource :: Source
defaultSource = Osc { freq : Num 60.0, sync : Num 0.1, offset : Num 0.0 }


defaultTexture :: Texture
defaultTexture = textureOf defaultSource


fromOp :: Operation -> Hydra
fromOp = Op


-- FIXME: use API for everything above


isValue :: Hydra -> Boolean
isValue (Val _) = true
isValue _ = false


isModifier :: Hydra -> Boolean
isModifier (Mod _) = true
isModifier _ = false


isBuffer :: Hydra -> Boolean
isBuffer (Buf _) = true
isBuffer _ = false


isTexture :: Hydra -> Boolean
isTexture (Tex _) = true
isTexture _ = false


isOp :: Hydra -> Boolean
isOp (Op _) = true
isOp _ = false


toValue :: Hydra -> Maybe Value
toValue (Val v) = Just v
toValue _ = Nothing


toBuffer :: Hydra -> Maybe Buffer
toBuffer (Buf buf) = Just buf
toBuffer _ = Nothing



toTexture :: Hydra -> Maybe Texture
toTexture (Tex t) = Just t
toTexture _ = Nothing


toModifier :: Hydra -> Maybe Modifier
toModifier (Mod m) = Just m
toModifier _ = Nothing


--addGeometry


type HydraFn1 = Hydra -> Hydra
type HydraFn2 = Hydra -> Hydra -> Hydra
type HydraFn3 = Hydra -> Hydra -> Hydra -> Hydra
type HydraFn4 = Hydra -> Hydra -> Hydra -> Hydra -> Hydra
type HydraFn5 = Hydra -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra
type HydraFn6 = Hydra -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra


type HydraFn1M = Maybe Hydra -> Maybe Hydra
type HydraFn2M = Maybe Hydra -> Maybe Hydra -> Maybe Hydra
type HydraFn3M = Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra
type HydraFn4M = Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra
type HydraFn5M = Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra
type HydraFn6M = Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra


type HydraTFn0 = HydraFn1
type HydraTFn1 = HydraFn2
type HydraTFn2 = HydraFn3
type HydraTFn3 = HydraFn4
type HydraTFn4 = HydraFn5
type HydraTFn5 = HydraFn6


type HydraEEFn0 = HydraFn2
type HydraEEFn1 = HydraFn3
type HydraEEFn2 = HydraFn4
type HydraEEFn3 = HydraFn5
type HydraEEFn4 = HydraFn6


type HydraTFn0M = HydraFn1M
type HydraTFn1M = HydraFn2M
type HydraTFn2M = HydraFn3M
type HydraTFn3M = HydraFn4M
type HydraTFn4M = HydraFn5M
type HydraTFn5M = HydraFn6M


type HydraTTFn0M = HydraFn2M
type HydraTTFn1M = HydraFn3M
type HydraTTFn2M = HydraFn4M
type HydraTTFn3M = HydraFn5M
type HydraTTFn4M = HydraFn6M


type ToHydraFn1 = Value -> Hydra
type ToHydraFn2 = Value -> Value -> Hydra
type ToHydraFn3 = Value -> Value -> Value -> Hydra
type ToHydraFn4 = Value -> Value -> Value -> Value -> Hydra
type ToHydraFn5 = Value -> Value -> Value -> Value -> Value -> Hydra
type ToHydraFn6 = Value -> Value -> Value -> Value -> Value -> Value -> Hydra


type ToHydraTFn0 = Texture -> Hydra
type ToHydraTFn1 = Texture -> Value -> Hydra
type ToHydraTFn2 = Texture -> Value -> Value -> Hydra
type ToHydraTFn3 = Texture -> Value -> Value -> Value -> Hydra
type ToHydraTFn4 = Texture -> Value -> Value -> Value -> Value -> Hydra
type ToHydraTFn5 = Texture -> Value -> Value -> Value -> Value -> Value -> Hydra


type ToHydraTTFn0 = Texture -> Texture -> Hydra
type ToHydraTTFn1 = Texture -> Texture -> Value -> Hydra
type ToHydraTTFn2 = Texture -> Texture -> Value -> Value -> Hydra
type ToHydraTTFn3 = Texture -> Texture -> Value -> Value -> Value -> Hydra
type ToHydraTTFn4 = Texture -> Texture -> Value -> Value -> Value -> Value -> Hydra


data TextureOrValue
    = T Texture
    | V Value


t' :: Texture -> TextureOrValue
t' = T


v' :: Value -> TextureOrValue
v' = V


textureOrValue :: forall x. (Texture -> x) -> (Value -> x) -> TextureOrValue -> x
textureOrValue tf _ (T t) = tf t
textureOrValue _ ev (V v) = ev v


instance ToFn Source Value where
    toFn (Noise vs)    = fn "noise" [ "scale" /\ vs.scale, "offset" /\ vs.offset ]
    toFn (Voronoi vs)  = fn "voronoi" [ "scale" /\ vs.scale, "speed" /\ vs.speed, "blending" /\ vs.blending ]
    toFn (Osc vs)      = fn "osc" [ "freq" /\ vs.freq, "sync" /\ vs.sync, "offset" /\ vs.offset ]
    toFn (Shape vs)    = fn "shape" [ "sides" /\ vs.sides, "radius" /\ vs.radius, "smoothing" /\ vs.smoothing ]
    toFn (Gradient vs) = fn "gradient" [ "speed" /\ vs.speed ]
    toFn (Solid vs)    = fn "solid" [ "r" /\ vs."r", "g" /\ vs.g, "b" /\ vs.b, "a" /\ vs.a ]
    toFn (Source _)    = fn "src" [] -- FIXME


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


instance ToFn Blend TextureOrValue where
    toFn (Add vs)   = fn "add" [ "what" /\ t' vs.what, "amount" /\ v' vs.amount ]
    toFn (Layer vs) = fn "layer" [ "what" /\ t' vs.what ]
    toFn (Blend vs) = fn "blend" [ "what" /\ t' vs.what, "amount" /\ v' vs.amount ]
    toFn (Mult vs)  = fn "mult" [ "what" /\ t' vs.what, "amount" /\ v' vs.amount ]
    toFn (Diff vs)  = fn "diff" [ "what" /\ t' vs.what ]
    toFn (Mask vs)  = fn "mask" [ "what" /\ t' vs.what, "reps" /\ v' vs.reps, "offset" /\ v' vs.offset ]


instance ToFn Modulate TextureOrValue where
    toFn (ModulateRepeat vs)   = fn "modulateRepeat"
                                    [ "what" /\ t' vs.what
                                    , "repeatX" /\ v' vs.repeatX, "repeatY" /\ v' vs.repeatY
                                    , "offsetX" /\ v' vs.offsetX, "offsetY" /\ v' vs.offsetY
                                    ]
    toFn (ModulateRepeatX vs)  = fn "modulateRepeatX"
                                    [ "what" /\ t' vs.what, "reps" /\ v' vs.reps, "offset" /\ v' vs.offset ]
    toFn (ModulateRepeatY vs)  = fn "modulateRepeatY"
                                    [ "what" /\ t' vs.what, "reps" /\ v' vs.reps, "offset" /\ v' vs.offset ]
    toFn (ModulateKaleid vs)   = fn "modulateKaleid"
                                    [ "what" /\ t' vs.what, "nSides" /\ v' vs.nSides ]
    toFn (ModulateScrollX vs)  = fn "modulateScrollX"
                                    [ "what" /\ t' vs.what, "scrollX" /\ v' vs.scrollX, "speed" /\ v' vs.speed ]
    toFn (ModulateScrollY vs)  = fn "modulateScrollY"
                                    [ "what" /\ t' vs.what, "scrollY" /\ v' vs.scrollY, "speed" /\ v' vs.speed ]
    toFn (Modulate vs)         = fn "modulate" [ "what" /\ t' vs.what, "amount" /\ v' vs.amount ]
    toFn (ModulateScale vs)    = fn "modulateScale"
                                    [ "what" /\ t' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulatePixelate vs) = fn "modulatePixelate"
                                    [ "what" /\ t' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulateRotate vs)   = fn "modulateRotate"
                                    [ "what" /\ t' vs.what, "multiple" /\ v' vs.multiple, "offset" /\ v' vs.offset ]
    toFn (ModulateHue vs)      = fn "modulateHue" [ "what" /\ t' vs.what, "amount" /\ v' vs.amount ]


instance ToFn Modifier TextureOrValue where
    toFn (G geom) = v' <$> toFn geom
    toFn (C color) = v' <$> toFn color
    toFn (B blend) = toFn blend
    toFn (M modulate) = toFn modulate


instance Show Operation where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"


instance Show Value where
    show (Num num) = show num
    show MouseX = "{mouse.x}"
    show MouseY = "{mouse.y}"
    show Time = "{time}"
    show CanvasWidth = "{width}"
    show CanvasHeight = "{height}"
    show WindowWidth = "{win.width}"
    show WindowHeight = "{win.height}"
    show (Seq values) = String.joinWith "," $ show <$> values
    show Pi = "{pi}"
    show (Expr v1 op v2) = "{" <> show v1 <> show op <> show v2 <> "}"
    show (OfTime v) = "{time -> " <> show v <> "}"
    show (Dynamic v) = "{* -> " <> show v <> "}"
    show (Harmonic n) = "{fft:" <> show n <> "}"
    show (Fast n v) = "{fast " <> show n <> " " <> show v <> "}"
    show (Sqrt n) = "{sqrt " <> show n <> "}"


instance Show Buffer where
    show Default = "def"
    show O0 = "o0"
    show O1 = "o1"
    show O2 = "o2"
    show O3 = "o3"
    show S0 = "s0"
    show S1 = "s1"
    show S2 = "s2"
    show S3 = "s3"


instance Show TextureOrValue where
    show (T texture) = show texture
    show (V value) = show value


instance Show Texture where
    show (Texture source modifiers) =
        show (toFn source :: Fn Value) <> "\n    " <>
            String.joinWith "\n    " (show <$> modifiers)


instance Show Modifier where
    show modifier =
        show $ (toFn modifier :: Fn TextureOrValue)


instance Show Hydra where
    show None = "None"
    show (Op op) = show op
    show (Val v) = show v
    show (Mod mod) = show mod
    show (Tex tex) = show tex
    show (Buf buf) = show buf