module Toolkit.Hydra2.Family.Geometry.FScroll where


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


id = Node.Family :: _ "scroll"


name :: String
name = "scroll"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speedX = Fn.Input :: _ "speedX"
_in_speedY = Fn.Input :: _ "speedY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, scrollX :: H.Value, scrollY :: H.Value, speedX :: H.Value, speedY :: H.Value )
type Outputs = ( out :: H.Texture )


type InputsOrder :: SOrder
type InputsOrder = "what" ::: "scrollX" ::: "scrollY" ::: "speedX" ::: "speedY" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


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
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
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