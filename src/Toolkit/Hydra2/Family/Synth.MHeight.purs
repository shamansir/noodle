module Toolkit.Hydra2.Family.Synth.FHeight where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "height"


name :: String
name = "height"


type State = Unit


defaultState :: State
defaultState = unit


_out_out = Fn.Output :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Height }


type Family (m :: Type -> Type) = -- {-> synth <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> synth <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
        $ P.send _out_out H.Height


type Node (m :: Type -> Type) =
    N.Node "height" State
        Inputs
        Outputs
        m