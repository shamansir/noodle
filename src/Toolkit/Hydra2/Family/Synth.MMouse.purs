module Toolkit.Hydra2.Family.Synth.FMouse where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure, discard)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "mouse"


name :: String
name = "mouse"


type State = Unit


defaultState :: State
defaultState = unit


_x_out = Fn.Output :: _ "x"
_y_out = Fn.Output :: _ "y"


type Inputs = ( )
type Outputs =
        ( x :: H.Value
        , y :: H.Value
        )


type InputsOrder :: SOrder
type InputsOrder = T


type OutputsOrder :: SOrder
type OutputsOrder = "x" ::: "y" ::: T


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
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            P.send _x_out H.MouseX
            P.send _y_out H.MouseY


type Node (m :: Type -> Type) =
    N.Node "mouse" State
        Inputs
        Outputs
        m