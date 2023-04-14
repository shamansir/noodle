module Toolkit.Hydra2.Family.Synth.FBpm where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "bpm"


name :: String
name = "bpm"


type State = Unit


defaultState :: State
defaultState = unit


_in_v = Fn.Input 1 :: _ "v"


type Inputs = ( v :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_v


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { v : H.Number 30.0 }


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
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            v <- P.receive _in_v
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "bpm" State
        Inputs
        Outputs
        m