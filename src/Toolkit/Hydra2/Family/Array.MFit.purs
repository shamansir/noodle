module Toolkit.Hydra2.Family.Array.FFit where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "fit"


name :: String
name = "fit"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr  = Fn.Input  1 :: _ "arr"
_in_low  = Fn.Input  2 :: _ "low"
_in_high = Fn.Input  3 :: _ "high"

_out_out = Fn.Output 1 :: _ "out"


type Inputs = ( arr :: H.VArray, low :: H.Value, high :: H.Value )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = s3 _in_arr _in_low _in_high


outputsOrder :: _
outputsOrder = s1 _out_out


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
            { inputs : inputsOrder, outputs : outputsOrder }
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