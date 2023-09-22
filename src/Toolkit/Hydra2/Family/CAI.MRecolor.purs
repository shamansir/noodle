module Toolkit.Hydra2.Family.CAI.FRecolor where

import Prelude

import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node

import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Fn as HFn


id = Node.Family :: _ "caiRecolor"


name :: String
name = "caiRecolor"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input  0 :: _ "what"
_in_with = Fn.Input  1 :: _ "with"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_what _in_with


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty  }


type Family (m :: Type -> Type) = -- {-> caiRecolor <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiRecolor <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            P.send _out_out
                $ H.CallGlslFn { over : what, mbWith : Just with }
                $ H.GlslFnRef
                $ HFn.empty "recolorCAI"


type Node (m :: Type -> Type) =
    N.Node "caiRecolor" State
        Inputs
        Outputs
        m