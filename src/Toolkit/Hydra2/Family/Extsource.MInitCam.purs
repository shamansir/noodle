module Toolkit.Hydra2.Family.Extsource.FInitCam where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_src = Fn.Input :: _ "src"
_in_index = Fn.Input :: _ "index"


type Inputs = ( src :: H.Source, index :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSource, index : H.Number 0.0 }


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
        $ Fn.make "initCam" $ do
            src <- P.receive _in_src
            index <- P.receive _in_index
            pure unit


type Node m =
    N.Node "initCam" Unit
        Inputs
        Outputs
        m