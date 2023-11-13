module Tookit.Hydra.Family.Color.FShift where


import Tookit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s5)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "shift"


name :: String
name = "shift"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input  0 :: _ "what"
_in_r    = Fn.Input  1 :: _ "r"
_in_g    = Fn.Input  2 :: _ "g"
_in_b    = Fn.Input  3 :: _ "b"
_in_a    = Fn.Input  4 :: _ "a"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, r :: H.Value, g :: H.Value, b :: H.Value, a :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s5 _in_what _in_r _in_g _in_b _in_a


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, r : H.Number 0.5, g : H.Number 0.5, b : H.Number 0.5, a : H.Number 0.5 }


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
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            P.send _out_out $ H.Filter what $ H.Shift { r, g, b, a }


type Node (m :: Type -> Type) =
    N.Node "shift" State
        Inputs
        Outputs
        m