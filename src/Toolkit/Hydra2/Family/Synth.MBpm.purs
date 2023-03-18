module Toolkit.Hydra2.Family.Synth.FBpm where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_v = Fn.Input :: _ "v"

_out_out = Fn.Output :: _ "out"


type Inputs = ( v :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { v : H.Number 30.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family m = -- {-> synth <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "bpm" $ do
            v <- P.receive _in_v
            -- TODO
            pure unit


type Node m =
    N.Node "bpm" Unit
        Inputs
        Outputs
        m