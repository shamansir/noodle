module Toolkit.Hydra.Family.Color.FG where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "g"


name :: String
name = "g"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_scale  = Fn.Input  1 :: _ "scale"
_in_offset = Fn.Input  2 :: _ "offset"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, scale :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_scale _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, scale : H.Number 1.0, offset : H.Number 0.0 }


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
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            P.send _out_out $ H.Filter what $ H.G { scale, offset }


type Node (m :: Type -> Type) =
    N.Node "g" State
        Inputs
        Outputs
        m