module Tookit.Hydra.Family.Modulate.FModulateRepeat where


import Tookit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s6)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "modulateRepeat"


name :: String
name = "modulateRepeat"


type State = Unit


defaultState :: State
defaultState = unit


_in_what    = Fn.Input  0 :: _ "what"
_in_with    = Fn.Input  1 :: _ "with"
_in_repeatX = Fn.Input  2 :: _ "repeatX"
_in_repeatY = Fn.Input  3 :: _ "repeatY"
_in_offsetX = Fn.Input  4 :: _ "offsetX"
_in_offsetY = Fn.Input  5 :: _ "offsetY"

_out_out    = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, repeatX :: H.Value, repeatY :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s6 _in_what _in_with _in_repeatX _in_repeatY _in_offsetX _in_offsetY


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, repeatX : H.Number 3.0, repeatY : H.Number 3.0, offsetX : H.Number 0.5, offsetY : H.Number 0.5 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
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
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModRepeat { repeatX, repeatY, offsetX, offsetY }


type Node (m :: Type -> Type) =
    N.Node "modulateRepeat" State
        Inputs
        Outputs
        m