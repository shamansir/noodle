module Tookit.Hydra.Family.Render.Cli.Node.Feed.CallGlslFunction where

import Prelude

import Type.Proxy (Proxy)
import Data.Number as Number
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Control.Monad.State (get) as State

import Data.Maybe (Maybe(..))
import Data.Array ((!!))
import Data.Array as Array
import Blessed.Internal.BlessedOp as BlessedOp

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (List)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift) as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.Core.Border as Border
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core as Core

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse, style) as List
import Blessed.UI.Lists.List.Property (selected) as List

import Tookit.Hydra.Lang.Glsl as Glsl
import Tookit.Hydra.Types as H
import Tookit.Hydra.Lang.Fn as Fn

import Noodle.Node (sendOut, sendOutM, atI, atIM, sendInM) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style
import Cli.Tagging as T
import Blessed.Tagger (fgc, s, render) as T

-- import Noodle.Node (Node)

import Tookit.Hydra.Types as T
-- import Tookit.Hydra.Family.Feed.FNumber (Inputs, Outputs, Node)
import Tookit.Hydra.Family.Feed.FCallGlslFunction (Node, State, _out_out, _idx_in, _p1_in, _p2_in, _p3_in, _p4_in, _p5_in) as FCallGlslFunction
-- import Tookit.Hydra (State) as Hydra

type ListKey = List <^> "call-fn-list"


-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> FCallGlslFunction.Node m -> BlessedOp FCallGlslFunction.State m
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
render nodeBoxKey node = do
    let
        (rootFnListKey :: ListKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        listKey = NK.append nodeBoxKey rootFnListKey
        glslFnList =
            B.listAnd listKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 20
                , Box.height $ Dimension.px $ Array.length Glsl.knownFns
                , Box.scrollable true
                , List.items $ (T.render <<< T.glslFnItem) <$> Glsl.knownFns
                , List.mouse true
                , List.keys true
                , Box.tags true
                , Style.glslFnList
                , Core.on List.Select
                    \_ _ -> do
                            selected <- List.selected ~< listKey
                            Node.sendInM node FCallGlslFunction._idx_in $ H.Number $ toNumber selected
                            {-
                            let mbSelectedFn = Glsl.knownFns !! selected
                            Blessed.lift $ case mbSelectedFn of
                                Just (H.GlslFn (_ /\ _ /\ fn)) -> do
                                    p1 <- Node.atIM node FCallGlslFunction._p1_in
                                    p2 <- Node.atIM node FCallGlslFunction._p2_in
                                    p3 <- Node.atIM node FCallGlslFunction._p3_in
                                    p4 <- Node.atIM node FCallGlslFunction._p4_in
                                    p5 <- Node.atIM node FCallGlslFunction._p5_in
                                    Node.sendOutM node FCallGlslFunction._out_out
                                        $ T.CallGlslFn
                                        $ T.GlslFnRef
                                        $ Fn.fnOf (Fn.name fn)
                                        $ Array.zipWith
                                            (\(name /\ _) val -> name /\ val)
                                            (Fn.args fn)
                                            [ p1, p2, p3, p4, p5 ]
                                    pure unit
                                Nothing -> pure unit
                            pure unit
                            -}

                        {-

                        selected <- List.selected ~< listKey
                        let mbSelectedFn = Glsl.knownFns !! selected
                        case mbSelectedFn of
                            Just fn -> do
                                Blessed.lift (pure unit :: m Unit)
                            -- Just fn -> liftEffect $ do
                                -- p1 <- Node.atI node FCallGlslFunction._p1_in
                                p1 <- Blessed.lift $ (Node.atI node FCallGlslFunction._p1_in :: m _)
                                Blessed.lift $ (Node.sendOutM (node :: FCallGlslFunction.Node m) FCallGlslFunction._out_out H.Empty :: m Unit)-- H.Empty
                                Blessed.lift $ Node.sendOut (node :: FCallGlslFunction.Node m) FCallGlslFunction._out_out H.Empty-- H.Empty
                            Nothing -> Blessed.lift (pure unit :: m Unit)
                        -}


                        {-
                        content <- TextArea.value ~< textBoxKey
                        let mbValues = T.findValues content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbValues of
                            Just values -> Node.sendOut node FArray._out_out values
                            Nothing -> pure unit
                        -}
                        -- (pure unit :: BlessedOp FCallGlslFunction.State Effect)
                ]
                [  ]
                $ \_ ->
                    (pure unit :: BlessedOp FCallGlslFunction.State Effect)
    nodeBoxKey >~ Node.append glslFnList




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit