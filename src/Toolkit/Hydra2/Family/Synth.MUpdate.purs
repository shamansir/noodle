module Toolkit.Hydra2.Family.Synth.FUpdate where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_fn = Fn.Input :: _ "fn"

_out_out = Fn.Output :: _ "out"


type Inputs = ( fn :: H.UpdateFn )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { fn : H.defaultUpdateFn }


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
        $ Fn.make "update" $ do
            fn <- P.receive _in_fn
            -- TODO
            pure unit


type Node m =
    N.Node "update" Unit
        Inputs
        Outputs
        m