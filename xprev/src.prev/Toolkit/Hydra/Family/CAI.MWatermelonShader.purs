module Toolkit.Hydra.Family.CAI.FWatermelonShader where

import Prelude


import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Data.SOrder (empty) as SOrder
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))


import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)
import Toolkit.Hydra.Lang.Fn as HFn


id = Node.Family :: _ "caiWatermelonShader"


name :: String
name = "caiWatermelonShader"


type State = Unit


defaultState :: State
defaultState = unit

_in_speed     = Fn.Input  0 :: _ "speed"
_in_intensity = Fn.Input  1 :: _ "intensity"

_out_shader = Fn.Output 0 :: _ "shader"


type Inputs = ( speed :: H.Value, intensity :: H.Value )
type Outputs = ( shader :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_speed _in_intensity


outputsOrder :: _
outputsOrder = s1 _out_shader


defaultInputs :: Record Inputs
defaultInputs = { speed : H.Number 1.0, intensity : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { shader : H.Empty }


type Family (m :: Type -> Type) = -- {-> caiWatermelonShader <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiWatermelonShader <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            speed <- P.receive _in_speed
            intensity <- P.receive _in_intensity
            P.send _out_shader
                $ H.CallGlslFn { over : H.Empty, mbWith : Nothing }
                $ H.GlslFnRef
                $ HFn.fn2 "watermelonCAI"
                    ( "speed" /\ H.V speed )
                    ( "intensity" /\ H.V intensity )


type Node (m :: Type -> Type) =
    N.Node "caiWatermelonShader" State
        Inputs
        Outputs
        WrapRepr
        m