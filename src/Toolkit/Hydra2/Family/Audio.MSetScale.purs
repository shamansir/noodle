module Toolkit.Hydra2.Family.Audio.FSetScale where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_audio = Fn.Input :: _ "audio"
_in_scale = Fn.Input :: _ "scale"


type Inputs = ( audio :: H.Audio, scale :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, scale : H.Number 10.0 }


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
        $ Fn.make "setScale" $ do
            audio <- P.receive _in_audio
            scale <- P.receive _in_scale
            pure unit


type Node m =
    N.Node "setScale" Unit
        Inputs
        Outputs
        m