module Toolkit.HydraGen.Types where -- FIXME: should be inside `HydraGen`

import Prelude
-- import Data.Array (Array)

import Effect (Effect)


type Context =
    { time :: Number
    -- , ...
    }

data Value
    = None
    | Number Number
    | VArray (Array Value) Ease
    | Dep (Context -> Number)
    -- | Fast Array Number
    -- | Smooth Array Number
    -- | ...
    | Time
    | MouseX
    | MouseY
    | Width
    | Height
    -- | ...
    | Harmonic AudioBin


type VArray = Array Value


data Texture
    = Solid
    | Dynamic
    | Video
    | Camera
    -- | ...

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
    = Mic
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