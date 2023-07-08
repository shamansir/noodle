module Toolkit.Hydra2.Family.Array.FOffset where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "offset"


name :: String
name = "offset"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr    = Fn.Input  0 :: _ "arr"
_in_offset = Fn.Input  1 :: _ "offset"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( arr :: H.Values, offset :: H.Value )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = s2 _in_arr _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, offset : H.Number 0.5 }


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
            offset <- P.receive _in_offset
            P.send _out_out $ H.VArray arr $ H.Offset offset


type Node (m :: Type -> Type) =
    N.Node "offset" State
        Inputs
        Outputs
        m