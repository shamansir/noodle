module Toolkit.Hydra.Family.Display.FInfo where

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)
import Toolkit.Hydra.Repr.Wrap (WrapRepr(..))


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "info"


name :: String
name = "info"


type State = H.Fn


defaultState :: State
defaultState = H.defaultFn


_in_in   = Fn.Input 0 :: _ "in"


type Inputs = ( in :: WrapRepr )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_in


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { in : Unit unit }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> info <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> info <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ pure unit


type Node (m :: Type -> Type) =
    N.Node "info" State
        Inputs
        Outputs
        WrapRepr
        m