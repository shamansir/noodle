module Toolkit.Hydra.Family.Modulate.FModulateScrollX where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s4)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "modulateScrollX"


name :: String
name = "modulateScrollX"


type State = Unit


defaultState :: State
defaultState = unit


_in_what    = Fn.Input  0 :: _ "what"
_in_with    = Fn.Input  1 :: _ "with"
_in_scrollX = Fn.Input  2 :: _ "scrollX"
_in_speed   = Fn.Input  3 :: _ "speed"

_out_out    = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, scrollX :: H.Value, speed :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s4 _in_what _in_with _in_scrollX _in_speed


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, scrollX : H.Number 0.5, speed : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModScrollX { scrollX, speed }


type Node (m :: Type -> Type) =
    N.Node "modulateScrollX" State
        Inputs
        Outputs
        WrapRepr
        m