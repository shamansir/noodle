module Toolkit.Hydra.Family.Geometry.FRepeat where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s5)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "repeat"


name :: String
name = "repeat"


type State = Unit


defaultState :: State
defaultState = unit


_in_what    = Fn.Input  0 :: _ "what"
_in_repeatX = Fn.Input  1 :: _ "repeatX"
_in_repeatY = Fn.Input  2 :: _ "repeatY"
_in_offsetX = Fn.Input  3 :: _ "offsetX"
_in_offsetY = Fn.Input  4 :: _ "offsetY"

_out_out    = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, repeatX :: H.Value, repeatY :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s5 _in_what _in_repeatX _in_repeatY _in_offsetX _in_offsetY


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, repeatX : H.Number 3.0, repeatY : H.Number 3.0, offsetX : H.Number 0.0, offsetY : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> geometry <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- Repeat what repeatX repeatY offsetX offsetY
            P.send _out_out $ H.Geometry what $ H.GRepeat { repeatX, repeatY, offsetX, offsetY }


type Node (m :: Type -> Type) =
    N.Node "repeat" State
        Inputs
        Outputs
        WrapRepr
        m