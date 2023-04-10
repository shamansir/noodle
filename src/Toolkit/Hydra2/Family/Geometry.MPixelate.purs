module Toolkit.Hydra2.Family.Geometry.FPixelate where


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


id = Node.Family :: _ "pixelate"


name :: String
name = "pixelate"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_pixelX = Fn.Input :: _ "pixelX"
_in_pixelY = Fn.Input :: _ "pixelY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, pixelX :: H.Value, pixelY :: H.Value )
type Outputs = ( out :: H.Texture )


type InputsOrder :: SOrder
type InputsOrder = "what" ::: "pixelX" ::: "pixelY" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, pixelX : H.Number 20.0, pixelY : H.Number 20.0 }


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
            pixelX <- P.receive _in_pixelX
            pixelY <- P.receive _in_pixelY
            -- Pixelate what pixelX pixelY
            P.send _out_out $ H.Geometry what $ H.GPixelate { pixelX, pixelY }


type Node (m :: Type -> Type) =
    N.Node "pixelate" State
        Inputs
        Outputs
        m