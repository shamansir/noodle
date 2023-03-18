module Toolkit.Hydra2.Family.Extsource.FInit where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_options = Fn.Input :: _ "options"


type Inputs = ( options :: H.SourceOptions )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { options : { src : H.Canvas } }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family m = -- {-> extsource <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "init" $ do
            options <- P.receive _in_options
            pure unit


type Node m =
    N.Node "init" Unit
        Inputs
        Outputs
        m