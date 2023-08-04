module Toolkit.Hydra2.Family.Feed.FPi where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N

import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "pi"


name :: String
name = "pi"


type State = Unit


defaultState :: State
defaultState = unit


_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Pi }


type Family (m :: Type -> Type) = -- {-> pi <-}
    Family.Def State
        Inputs
        Outputs
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> pi <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            -- Pi
            P.send _out_out $ H.Pi


type Node (m :: Type -> Type) =
    N.Node "pi" State
        Inputs
        Outputs
        m