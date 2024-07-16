module Toolkit.Hydra.Family.Synth.FHush where


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
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "hush"


name :: String
name = "hush"


type State = Unit


defaultState :: State
defaultState = unit


_in_todo = Fn.Input  0 :: _ "todo"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( todo :: H.TODO )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_todo


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { todo : H.TODO }


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
            _ <- P.receive _in_todo
            pure unit


type Node (m :: Type -> Type) =
    N.Node "hush" State
        Inputs
        Outputs
        WrapRepr
        m