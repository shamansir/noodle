module Toolkit.Hydra2.Family.CAI.FProductGradient where

import Prelude


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "caiProductGradient"


name :: String
name = "caiProductGradient"


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


type Family (m :: Type -> Type) = -- {-> caiProductGradient <-}
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
            product <- P.receive _in_product
            P.send _out_gradient $ H.Start $ H.Gradient { speed : H.Number 1.0 }


type Node (m :: Type -> Type) =
    N.Node "caiProductGradient" State
        Inputs
        Outputs
        m
