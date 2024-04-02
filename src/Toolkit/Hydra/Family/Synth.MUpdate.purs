module Toolkit.Hydra.Family.Synth.FUpdate where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "update"


name :: String
name = "update"


type State = Unit


defaultState :: State
defaultState = unit


_in_fn = Fn.Input 0 :: _ "fn"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( fn :: H.UpdateFn )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_fn


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { fn : H.defaultUpdateFn }


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
            fn <- P.receive _in_fn
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "update" State
        Inputs
        Outputs
        m