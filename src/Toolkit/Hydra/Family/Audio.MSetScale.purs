module Tookit.Hydra.Family.Audio.FSetScale where


import Tookit.Hydra.Types as H


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


id = Node.Family :: _ "setScale"


name :: String
name = "setScale"


type State = Unit


defaultState :: State
defaultState = unit


_in_scale = Fn.Input 0 :: _ "scale"


type Inputs = ( scale :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_scale


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { scale : H.Number 10.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> audio <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> audio <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            scale <- P.receive _in_scale
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setScale" State
        Inputs
        Outputs
        m