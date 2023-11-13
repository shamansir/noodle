module Tookit.Hydra.Family.Feed.FExpression where

import Tookit.Hydra.Types as H


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


id = Node.Family :: _ "expression"


name :: String
name = "expression"


type State = H.Fn


defaultState :: State
defaultState = H.defaultFn


_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Fn )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.defaultFn }


type Family (m :: Type -> Type) = -- {-> expression <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> expression <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ pure unit
            -- fnValue <- P.receive _in_in
            -- P.send _out_out fnValue


type Node (m :: Type -> Type) =
    N.Node "expression" State
        Inputs
        Outputs
        m