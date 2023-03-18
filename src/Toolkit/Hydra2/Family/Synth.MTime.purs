module Toolkit.Hydra2.Family.Synth.FTime where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N



_out_out = Fn.Output :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Time }


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
        $ Fn.make "time" $ do
            -- Time
            P.send _out_out H.Time


type Node m =
    N.Node "time" Unit
        Inputs
        Outputs
        m