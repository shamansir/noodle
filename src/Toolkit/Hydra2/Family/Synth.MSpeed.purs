module Toolkit.Hydra2.Family.Synth.FSpeed where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "speed"


name :: String
name = "speed"


type State = Unit


defaultState :: State
defaultState = unit


_in_v = Fn.Input :: _ "v"


type Inputs = ( v :: H.Value )
type Outputs = ( )


type InputsOrder :: SOrder
type InputsOrder = "v" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = T


defaultInputs :: Record Inputs
defaultInputs = { v : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


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
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            v <- P.receive _in_v
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "speed" State
        Inputs
        Outputs
        m