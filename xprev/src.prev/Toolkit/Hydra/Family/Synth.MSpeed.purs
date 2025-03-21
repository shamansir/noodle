module Toolkit.Hydra.Family.Synth.FSpeed where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Type.Proxy (Proxy(..))
import Data.SOrder (empty) as SOrder


id = Node.Family :: _ "speed"


name :: String
name = "speed"


type State = Unit


defaultState :: State
defaultState = unit


_in_v = Fn.Input 0 :: _ "v"


type Inputs = ( v :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_v


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { v : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> synth <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
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
    N.Node "speed" State
        Inputs
        Outputs
        WrapRepr
        m