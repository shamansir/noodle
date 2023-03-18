module Toolkit.Hydra2.Family.Out.FOut where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_what = Fn.Input :: _ "what"
_in_target = Fn.Input :: _ "target"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, target :: H.Output )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, target : H.Screen }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family m = -- {-> out <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> out <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "out" $ do
            what <- P.receive _in_what
            target <- P.receive _in_target
            pure unit


type Node m =
    N.Node "out" Unit
        Inputs
        Outputs
        m