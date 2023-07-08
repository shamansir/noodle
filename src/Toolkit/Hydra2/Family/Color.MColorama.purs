module Toolkit.Hydra2.Family.Color.FColorama where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "colorama"


name :: String
name = "colorama"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_amount = Fn.Input  1 :: _ "amount"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, amount :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_what _in_amount


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, amount : H.Number 0.005 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            P.send _out_out $ H.WithColor what $ H.Colorama amount


type Node (m :: Type -> Type) =
    N.Node "colorama" State
        Inputs
        Outputs
        m