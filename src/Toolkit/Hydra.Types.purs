module Toolkit.HydraGen.Types where -- FIXME: should be inside `HydraGen`

import Prelude

import Effect (Effect)


data TODO = TODO


type Context =
    { time :: Number
    -- , ...
    }

data Value
    = None
    | Required -- a.k.a. Undefined
    | Number Number
    | VArray (Array Value) Ease
    | Dep (Context -> Number)
    -- | ...
    | Time
    | MouseX
    | MouseY
    | Width
    | Height
    -- | ...
    | Audio Audio AudioBin


type VArray = Array Value


data Texture
    = Empty
    | From Source
    | BlendOf { what :: Texture, with :: Texture } Blend
    | WithColor Texture ColorOp
    | ModulateWith { what :: Texture, with :: Texture } Modulate
    | Geometry Texture Geometry


data Source
    = Dynamic
    | Video
    | Camera
    | Gradient { speed :: Value }
    | Noise { scale :: Value, offset :: Value }
    | Osc { frequency :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }
    | Source From -- Output?
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }
    -- | ..


data Blend
    = Blend Value -- amount
    | Add Value -- amount
    | Diff
    | Layer Value -- amount
    | Mask
    | Mult Value -- amount
    | Sub Value -- amount


data ColorOp
    = R { scale :: Value, offset :: Value }
    | G { scale :: Value, offset :: Value }
    | B { scale :: Value, offset :: Value }
    | A { scale :: Value, offset :: Value }
    | Posterize { bins :: Value, gamma :: Value }
    | Shift { r :: Value, g :: Value, b :: Value, a :: Value }
    | Invert Value -- amount
    | Contrast Value -- amount
    | Brightness Value -- amount
    | Luma { treshold :: Value, tolerance :: Value }
    | Tresh { treshold :: Value, tolerance :: Value }
    | Color { r :: Value, g :: Value, b :: Value, a :: Value }
    | Saturate Value -- amount
    | Hue Value -- amount
    | Colorama Value -- amount


data Modulate
    = Modulate Value -- amount
    | ModHue Value -- amount
    | ModKaleid { nSides :: Value }
    | ModPixelate { multiple :: Value, offset :: Value }
    | ModRepeat { offsetX :: Value, offsetY :: Value, repeatX :: Value, repeatY :: Value }
    | ModRepeatX { offset :: Value, reps :: Value } -- TODO: join with `ModRepeat`
    | ModRepeatY { offset :: Value, reps :: Value } -- TODO: join with `ModRepeat`
    | ModRotate { multiple :: Value, offset :: Value }
    | ModScale { multiple :: Value, offset :: Value }
    | ModScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | ModScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | ModScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data Geometry
    = GKaleid { nSides :: Value }
    | GPixelate { pixelX :: Value, pixelY :: Value }
    | GRepeat { offsetX :: Value, offsetY :: Value, repeatX :: Value, repeatY :: Value }
    | GRepeatX { offset :: Value, reps :: Value } -- TODO: join with `Repeat`
    | GRepeatY { offset :: Value, reps :: Value } -- TODO: join with `Repeat`
    | GRotate { angle :: Value, speed :: Value }
    | GScale { amount :: Value, xMult :: Value, yMult :: Value, offsetX :: Value, offsetY :: Value }
    | GScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | GScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | GScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data Output
    = Screen
    | Output0
    | Output1
    | Output2
    -- | ...


data Ease
    = Linear
    | Fast Value
    | Smooth Value
    | Fit Value Value
    | Offset Value
    | InOutCubic
    -- | ...


data From
    = All
    | Output Output

data Audio
    = Silence
    | Mic
    | File
    -- | ...

data AudioBin
    = H0
    | H1
    | H2
    | H3
    | H4
    -- ..

type UpdateFn = Context -> Effect Unit


data Canvas = Canvas


type SourceOptions =
    { src :: Canvas
    -- , ...
    }

type GlslFn = Unit -- TODO


noValues :: VArray
noValues = []