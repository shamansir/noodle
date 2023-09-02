module Toolkit.Hydra2.Family.CAI.FGradientShader where

import Prelude


import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node

import Toolkit.Hydra2.Types as H


id = Node.Family :: _ "caiGradientShader"


name :: String
name = "caiGradientShader"


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
defaultInputs = { speed : H.Number 1.0, intensity : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { shader : H.Empty }


type Family (m :: Type -> Type) = -- {-> caiGradientShader <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiProductGradient <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            speed <- P.receive _in_speed
            intensity <- P.receive _in_intensity
            P.send _out_shader $ H.Empty -- FIXME


type Node (m :: Type -> Type) =
    N.Node "caiGradientShader" State
        Inputs
        Outputs
        m