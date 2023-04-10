module Toolkit.Hydra2.Family.Array.FFit where


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


id = Node.Family :: _ "fit"


name :: String
name = "fit"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr = Fn.Input :: _ "arr"
_in_low = Fn.Input :: _ "low"
_in_high = Fn.Input :: _ "high"

_out_out = Fn.Output :: _ "out"


type Inputs = ( arr :: H.VArray, low :: H.Value, high :: H.Value )
type Outputs = ( out :: H.Value )


type InputsOrder :: SOrder
type InputsOrder = "arr" ::: "low" ::: "hight" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, low : H.Number 0.0, high : H.Number 1.1 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.None }


type Family (m :: Type -> Type) = -- {-> array <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> array <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            arr <- P.receive _in_arr
            low <- P.receive _in_low
            high <- P.receive _in_high
            P.send _out_out $ H.VArray arr $ H.Fit { low, high }


type Node (m :: Type -> Type) =
    N.Node "fit" State
        Inputs
        Outputs
        m