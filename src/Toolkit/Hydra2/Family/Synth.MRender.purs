module Toolkit.Hydra2.Family.Synth.FRender where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "render"


name :: String
name = "render"


_in_from = Fn.Input :: _ "from"


type Inputs = ( from :: H.From )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { from : H.All }


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
        $ Fn.make name $ do
            from <- P.receive _in_from
            -- TODO
            pure unit


type Node m =
    N.Node "render" Unit
        Inputs
        Outputs
        m