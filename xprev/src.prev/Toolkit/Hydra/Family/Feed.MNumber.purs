module Toolkit.Hydra.Family.Feed.FNumber where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


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


id = Node.Family :: _ "number"


name :: String
name = "number"


type State = Number


defaultState :: State
defaultState = 0.0


_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Number 0.0 }


type Family (m :: Type -> Type) = -- {-> number <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> number <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ pure unit
            -- numberValue <- P.receive _in_in
            -- P.send _out_out numberValue


type Node (m :: Type -> Type) =
    N.Node "number" State
        Inputs
        Outputs
        WrapRepr
        m