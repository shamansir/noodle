module Toolkit.Hydra2.Family.CAI.FProductRecolor where

import Prelude


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "caiProductRecolor"


name :: String
name = "caiProductRecolor"


type State = Unit


defaultState :: State
defaultState = unit


_in_texture   = Fn.Input  0 :: _ "texture"
_in_product   = Fn.Input  1 :: _ "product"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( texture :: H.Texture, product :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_texture _in_product


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { texture : H.Empty, product : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty  }


type Family (m :: Type -> Type) = -- {-> caiProductRecolor <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiProductRecolor <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            texture <- P.receive _in_texture
            product <- P.receive _in_product
            P.send _out_out $ H.Empty -- FIXME


type Node (m :: Type -> Type) =
    N.Node "caiProductRecolor" State
        Inputs
        Outputs
        m