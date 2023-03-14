module Toolkit.HydraGen.Types where -- FIXME: should be inside `HydraGen`

import Prelude
-- import Data.Array (Array)

import Effect (Effect)


data TODO = TODO


type Context =
    { time :: Number
    -- , ...
    }

data Value
    = None
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
    -- | ...

data Blend
    = Blend Value
    | Add Value
    | Diff
    | Layer Value
    | Mask
    | Mult Value
    | Sub Value


data Texture
    = Empty
    | From TextureSource
    | BlendOf Texture Texture Blend


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