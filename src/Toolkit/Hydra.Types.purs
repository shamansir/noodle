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
    | Required
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


data TextureSource
    = Solid
    | Dynamic
    | Video
    | Camera
    -- | Osc ...
    -- | Noise ...
    -- | ...

data Blend
    = Blend Value
    | Add Value
    | Diff
    | Layer Value
    | Mask
    | Mult Value
    | Sub Value


data ColorOp
    = R { scale :: Value, offset :: Value }
    | G { scale :: Value, offset :: Value }
    | B { scale :: Value, offset :: Value }
    | A { scale :: Value, offset :: Value }
    | Posterize { bins :: Value, gamma :: Value }
    | Shift { r :: Value, g :: Value, b :: Value, a :: Value }
    | Invert Value
    | Contrast Value
    | Brightness Value
    | Luma { treshold :: Value, tolerance :: Value }
    | Tresh { treshold :: Value, tolerance :: Value }
    | Color { r :: Value, g :: Value, b :: Value, a :: Value }
    | Saturate Value
    | Hue Value
    | Colorama Value


data Texture
    = Empty
    | From TextureSource
    | BlendOf { what :: Texture, with :: Texture } Blend
    | WithColor Texture ColorOp


data Source
    = Source0
    | Source1
    | Source2
    -- | ...

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