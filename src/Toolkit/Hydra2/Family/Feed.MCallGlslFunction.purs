module Toolkit.Hydra2.Family.Feed.FCallGlslFunction where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Int (floor)

import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Glsl as Glsl

import Data.Tuple.Nested ((/\))
import Data.SOrder (SOrder, type (:::), T, s1, s6)
import Data.SOrder (empty) as SOrder
import Data.Array (zipWith) as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..))

import Noodle.Id (Input(..), Output(..)) as Fn

import Noodle.Family.Def as Family
import Noodle.Node2 (Node, atIM) as N
import Noodle.Node2 (atIM) as Node
import Noodle.Id (Family(..)) as Node
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P

import Toolkit.Hydra2.Lang.Fn as HFn
import Toolkit.Hydra2.Types as H


import Type.Proxy (Proxy(..))


id = Node.Family :: _ "callGlslFn"


name :: String
name = "callFunction"


type State = H.Fn


defaultState :: State
defaultState = H.defaultFn


_idx_in   = Fn.Input 0 :: _ "idx"
_p1_in   = Fn.Input 1 :: _ "p1"
_p2_in   = Fn.Input 2 :: _ "p2"
_p3_in   = Fn.Input 3 :: _ "p3"
_p4_in   = Fn.Input 4 :: _ "p4"
_p5_in   = Fn.Input 5 :: _ "p5"


_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( idx :: H.Value, p1 :: H.GlslFnArg, p2 :: H.GlslFnArg, p3 :: H.GlslFnArg, p4 :: H.GlslFnArg, p5 :: H.GlslFnArg )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s6 _idx_in _p1_in _p2_in _p3_in _p4_in _p5_in


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs =
    { idx : H.None
    , p1 : H.defaultGlslFnArg
    , p2 : H.defaultGlslFnArg
    , p3 : H.defaultGlslFnArg
    , p4 : H.defaultGlslFnArg
    , p5 : H.defaultGlslFnArg
    }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


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
            $ do
                mbIndex <- P.receive _idx_in
                case mbIndex of
                    H.Number n -> do
                        fnRef <- collectFuncRef $ floor n
                        P.send _out_out $ H.CallGlslFn fnRef
                    _ -> pure unit

                pure unit
            -- fnValue <- P.receive _in_in
            -- P.send _out_out fnValue


type Node (m :: Type -> Type) =
    N.Node "callGlslFn" State
        Inputs
        Outputs
        m


collectFuncRef :: forall m. Int -> P.ProcessM State Inputs Outputs m H.GlslFnRef
collectFuncRef index = do
    let mbSelectedFn = Glsl.knownFns !! index
    case mbSelectedFn of
        Just (H.GlslFn (_ /\ _ /\ fn)) -> do
            p1 <- P.receive _p1_in
            p2 <- P.receive _p2_in
            p3 <- P.receive _p3_in
            p4 <- P.receive _p4_in
            p5 <- P.receive _p5_in
            pure
                $ H.GlslFnRef
                $ HFn.fnOf (HFn.name fn)
                $ Array.zipWith
                    (\(name /\ _) val -> name /\ val)
                    (HFn.args fn)
                    [ p1, p2, p3, p4, p5 ]
        Nothing -> pure $ H.defaultGlslFnRef


collectFuncRef'
    :: forall f m m'
     . Bind m
    => MonadEffect m
    => Int
    -> N.Node f
        State
        Inputs
        Outputs
        m'
    -> m H.GlslFnRef
collectFuncRef' index node = do
    let mbSelectedFn = Glsl.knownFns !! index
    case mbSelectedFn of
        Just (H.GlslFn (_ /\ _ /\ fn)) -> do
            p1 <- Node.atIM node _p1_in
            p2 <- Node.atIM node _p2_in
            p3 <- Node.atIM node _p3_in
            p4 <- Node.atIM node _p4_in
            p5 <- Node.atIM node _p5_in
            pure
                $ H.GlslFnRef
                $ HFn.fnOf (HFn.name fn)
                $ Array.zipWith
                    (\(name /\ _) val -> name /\ val)
                    (HFn.args fn)
                    [ p1, p2, p3, p4, p5 ]
        Nothing -> pure $ H.defaultGlslFnRef