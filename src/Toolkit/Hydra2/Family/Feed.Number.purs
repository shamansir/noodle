module Toolkit.Hydra2.Family.Feed.Number where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "number"


name :: String
name = "number"


type State = Number


defaultState :: State
defaultState = 0.0


_in_in   = Fn.Input 1 :: _ "in" -- TODO: make hidden

_out_out   = Fn.Output 1 :: _ "out"


type Inputs = ( in :: H.Value )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = s1 _in_in


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { in : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Number 0.0 }


type Family (m :: Type -> Type) = -- {-> number <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> number <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            numberValue <- P.receive _in_in
            P.send _out_out numberValue


type Node (m :: Type -> Type) =
    N.Node "number" State
        Inputs
        Outputs
        m