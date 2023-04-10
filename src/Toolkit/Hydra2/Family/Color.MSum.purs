module Toolkit.Hydra2.Family.Color.FSum where


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


id = Node.Family :: _ "sum"


name :: String
name = "sum"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"


_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, todo :: H.TODO )
type Outputs = ( )


type InputsOrder :: SOrder
type InputsOrder = "what" ::: "todo" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = T


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, todo : H.TODO }
        { }
        $ Fn.make name
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            what <- P.receive _in_what
            pure unit


type Node (m :: Type -> Type) =
    N.Node "sum" State
        Inputs
        Outputs
        m