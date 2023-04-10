module Toolkit.Hydra2.Family.Synth.FPi where


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


id = Node.Family :: _ "pi"


name :: String
name = "pi"


type State = Unit


defaultState :: State
defaultState = unit


_out_out = Fn.Output :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Value )


type InputsOrder :: SOrder
type InputsOrder = T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Pi }


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
            -- Pi
            P.send _out_out $ H.Pi


type Node (m :: Type -> Type) =
    N.Node "pi" State
        Inputs
        Outputs
        m