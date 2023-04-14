module Toolkit.Hydra2.Family.Synth.FMouse where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure, discard)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "mouse"


name :: String
name = "mouse"


type State = Unit


defaultState :: State
defaultState = unit


_out_x = Fn.Output 1 :: _ "x"
_out_y = Fn.Output 2 :: _ "y"


type Inputs = ( )
type Outputs =
        ( x :: H.Value
        , y :: H.Value
        )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s2 _out_x _out_y


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { x : H.MouseX, y : H.MouseY }


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
            P.send _out_x H.MouseX
            P.send _out_y H.MouseY


type Node (m :: Type -> Type) =
    N.Node "mouse" State
        Inputs
        Outputs
        m