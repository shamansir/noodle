module Toolkit.Hydra2.Family.CAI.FSmartGradient where


import Prelude (Unit, unit, ($), bind, pure)

import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))

import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Fn as HFn


id = Node.Family :: _ "caiSmartGradient"


name :: String
name = "caiSmartGradient"


type State = Unit


defaultState :: State
defaultState = unit


_in_texture   = Fn.Input  0 :: _ "texture"
_in_product   = Fn.Input  1 :: _ "product"

_out_gradient   = Fn.Output 0 :: _ "gradient"


type Inputs = ( product :: H.Value )
type Outputs = ( gradient :: H.Texture )


inputsOrder :: _
inputsOrder = s1 _in_product


outputsOrder :: _
outputsOrder = s1 _out_gradient


defaultInputs :: Record Inputs
defaultInputs = { product : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { gradient : H.Empty }


type Family (m :: Type -> Type) = -- {-> caiSmartGradient <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiSmartGradient <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            product <- P.receive _in_product
            let
                color0 = H.Start $ H.Solid { r : H.Number 1.0, g : H.Number 0.0, b : H.Number 0.0, a : H.Number 1.0 }
                color1 = H.Start $ H.Solid { r : H.Number 0.0, g : H.Number 1.0, b : H.Number 0.0, a : H.Number 1.0 }
                color2 = H.Start $ H.Solid { r : H.Number 0.0, g : H.Number 0.0, b : H.Number 1.0, a : H.Number 1.0 }
            P.send _out_gradient
                $ H.CallGlslFn H.Empty
                $ H.GlslFnRef
                $ HFn.fn3 "gradient3CAI"
                    ( "primary" /\ H.T color0 )
                    ( "secondary" /\ H.T color1 )
                    ( "ternary" /\ H.T color2 )


type Node (m :: Type -> Type) =
    N.Node "caiSmartGradient" State
        Inputs
        Outputs
        m
