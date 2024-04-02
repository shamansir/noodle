module Toolkit.Hydra.Family.Source.FNoise where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "noise"


name :: String
name = "noise"


type State = Unit


defaultState :: State
defaultState = unit


_in_scale  = Fn.Input  0 :: _ "scale"
_in_offset = Fn.Input  1 :: _ "offset"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( scale :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_scale _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { scale : H.Number 10.0, offset : H.Number 0.1 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> source <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> source <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            P.send _out_out $ H.Start $ H.Noise { scale, offset }


type Node (m :: Type -> Type) =
    N.Node "noise" State
        Inputs
        Outputs
        m