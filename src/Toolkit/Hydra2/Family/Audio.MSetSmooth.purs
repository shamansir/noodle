module Toolkit.Hydra2.Family.Audio.FSetSmooth where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "setSmooth"


name :: String
name = "setSmooth"


_in_audio = Fn.Input :: _ "audio"
_in_smooth = Fn.Input :: _ "smooth"


type Inputs = ( audio :: H.Audio, smooth :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, smooth : H.Number 0.4 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family m = -- {-> audio <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            audio <- P.receive _in_audio
            smooth <- P.receive _in_smooth
            pure unit


type Node m =
    N.Node "setSmooth" Unit
        Inputs
        Outputs
        m