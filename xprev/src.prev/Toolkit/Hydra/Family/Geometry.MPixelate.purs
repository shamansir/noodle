module Toolkit.Hydra.Family.Geometry.FPixelate where


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
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "pixelate"


name :: String
name = "pixelate"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_pixelX = Fn.Input  1 :: _ "pixelX"
_in_pixelY = Fn.Input  2 :: _ "pixelY"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, pixelX :: H.Value, pixelY :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_pixelX _in_pixelY


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, pixelX : H.Number 20.0, pixelY : H.Number 20.0 }


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
            pixelX <- P.receive _in_pixelX
            pixelY <- P.receive _in_pixelY
            -- Pixelate what pixelX pixelY
            P.send _out_out $ H.Geometry what $ H.GPixelate { pixelX, pixelY }


type Node (m :: Type -> Type) =
    N.Node "pixelate" State
        Inputs
        Outputs
        WrapRepr
        m