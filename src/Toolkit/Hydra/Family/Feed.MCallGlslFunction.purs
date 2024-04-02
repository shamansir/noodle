module Toolkit.Hydra.Family.Feed.FCallGlslFunction where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Int (floor)

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Lang.Glsl as Glsl

import Data.Tuple.Nested ((/\))
import Data.SOrder (SOrder, type (:::), T, s1, s7)
import Data.SOrder (empty) as SOrder
import Data.Array (zipWith) as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..))

import Noodle.Id (Input(..), Output(..)) as Fn

import Noodle.Family.Def as Family
import Noodle.Node (Node, atIM) as N
import Noodle.Node (atIM) as Node
import Noodle.Id (Family(..)) as Node
import Noodle.Fn as Fn
import Noodle.Fn.Process as P

import Toolkit.Hydra.Lang.Fn as HFn
import Toolkit.Hydra.Types as H


import Type.Proxy (Proxy(..))


id = Node.Family :: _ "callFunction"


name :: String
name = "callFunction"


type State = H.Fn


defaultState :: State
defaultState = H.defaultFn


_tex_in  = Fn.Input 0 :: _ "tex"
_idx_in  = Fn.Input 1 :: _ "idx"
_p1_in   = Fn.Input 2 :: _ "p1"
_p2_in   = Fn.Input 3 :: _ "p2"
_p3_in   = Fn.Input 4 :: _ "p3"
_p4_in   = Fn.Input 5 :: _ "p4"
_p5_in   = Fn.Input 6 :: _ "p5"


_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( tex :: H.Texture, idx :: H.Value, p1 :: H.GlslFnArg, p2 :: H.GlslFnArg, p3 :: H.GlslFnArg, p4 :: H.GlslFnArg, p5 :: H.GlslFnArg )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s7 _tex_in _idx_in _p1_in _p2_in _p3_in _p4_in _p5_in


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs =
    { idx : H.None
    , tex : H.Empty
    , p1 : H.defaultGlslFnArg
    , p2 : H.defaultGlslFnArg
    , p3 : H.defaultGlslFnArg
    , p4 : H.defaultGlslFnArg
    , p5 : H.defaultGlslFnArg
    }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> callFunction <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> callFunction <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
                vIndex <- P.receive _idx_in
                case vIndex of
                    H.Number n -> do
                        fnRef <- collectFuncRef $ floor n
                        tex <- P.receive _tex_in
                        P.send _out_out $ H.CallGlslFn { over : tex, mbWith : Nothing } fnRef
                    _ -> pure unit

                pure unit
            -- fnValue <- P.receive _in_in
            -- P.send _out_out fnValue


type Node (m :: Type -> Type) =
    N.Node "callFunction" State
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