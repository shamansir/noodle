module Toolkit.Hydra2.Family.Feed.FCallGlslFunction where

import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s5)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "callGlslFn"


name :: String
name = "callFunction"


type State = H.Fn


defaultState :: State
defaultState = H.defaultFn


_p1_in   = Fn.Input 0 :: _ "arg1"
_p2_in   = Fn.Input 0 :: _ "arg2"
_p3_in   = Fn.Input 0 :: _ "arg3"
_p4_in   = Fn.Input 0 :: _ "arg4"
_p5_in   = Fn.Input 0 :: _ "arg5"


_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( p1 :: H.ShaderFnArg, p2 :: H.ShaderFnArg, p3 :: H.ShaderFnArg, p4 :: H.ShaderFnArg, p5 :: H.ShaderFnArg )
type Outputs = ( out :: H.ShaderFn )


inputsOrder :: _
inputsOrder = s5 _p1_in _p2_in _p3_in _p4_in _p5_in


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs =
    { p1 : H.defaultShaderFnArg
    , p2 : H.defaultShaderFnArg
    , p3 : H.defaultShaderFnArg
    , p4 : H.defaultShaderFnArg
    , p5 : H.defaultShaderFnArg
    }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.defaultShaderFn }


type Family (m :: Type -> Type) = -- {-> callGlslFn <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> callGlslFn <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ pure unit
            -- fnValue <- P.receive _in_in
            -- P.send _out_out fnValue


type Node (m :: Type -> Type) =
    N.Node "callGlslFn" State
        Inputs
        Outputs
        m