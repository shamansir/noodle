module Toolkit.Hydra2.Family.Synth.FSetFunction where


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


id = Node.Family :: _ "setFunction"


name :: String
name = "setFunction"


type State = Unit


defaultState :: State
defaultState = unit


_in_fn = Fn.Input 0 :: _ "fn"


type Inputs = ( fn :: H.GlslFn )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_fn


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { fn : H.defaultGlslFn }


defaultOutputs :: Record Outputs
defaultOutputs = { }


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
            fn <- P.receive _in_fn
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setFunction" State
        Inputs
        Outputs
        m