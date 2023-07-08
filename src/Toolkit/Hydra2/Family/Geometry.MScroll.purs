module Toolkit.Hydra2.Family.Geometry.FScroll where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s5)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "scroll"


name :: String
name = "scroll"


type State = Unit


defaultState :: State
defaultState = unit


_in_what    = Fn.Input  0 :: _ "what"
_in_scrollX = Fn.Input  1 :: _ "scrollX"
_in_scrollY = Fn.Input  2 :: _ "scrollY"
_in_speedX  = Fn.Input  3 :: _ "speedX"
_in_speedY  = Fn.Input  4 :: _ "speedY"

_out_out    = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, scrollX :: H.Value, scrollY :: H.Value, speedX :: H.Value, speedY :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s5 _in_what _in_scrollX _in_scrollY _in_speedX _in_speedY


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, scrollX : H.Number 0.5, scrollY : H.Number 0.5, speedX : H.Number 1.0, speedY : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def State
        Inputs
        Outputs
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
            scrollX <- P.receive _in_scrollX
            scrollY <- P.receive _in_scrollY
            speedX <- P.receive _in_speedX
            speedY <- P.receive _in_speedY
            P.send _out_out $ H.Geometry what $ H.GScroll { scrollX, scrollY, speedX, speedY }


type Node (m :: Type -> Type) =
    N.Node "scroll" State
        Inputs
        Outputs
        m