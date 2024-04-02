module Toolkit.Hydra.Family.Array.FEase where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2, s1)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "ease"


name :: String
name = "ease"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr  = Fn.Input  0 :: _ "arr"
_in_ease = Fn.Input  1 :: _ "ease"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( arr :: H.Values, ease :: H.Ease )
type Outputs = ( out :: H.Value )


inputsOrder ∷ _
inputsOrder = s2 _in_arr _in_ease

outputsOrder ∷ _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, ease : H.Linear }


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
        { arr : H.noValues, ease : H.Linear }
        { out : H.None }
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            arr <- P.receive _in_arr
            ease <- P.receive _in_ease
            P.send _out_out $ H.VArray arr ease


type Node (m :: Type -> Type) =
    N.Node "ease" State
        Inputs
        Outputs
        m