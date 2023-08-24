module Toolkit.Hydra2.Family.Render.Cli.Node.Feed.CallGlslFunction where

import Prelude

import Type.Proxy (Proxy)
import Data.Number as Number

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Data.Maybe (Maybe(..))

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

import Toolkit.Hydra2.Lang.Glsl as Glsl

import Noodle.Node2 (sendOut) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style
import Cli.Tagging as T
import Blessed.Tagger (fgc, s, render) as T

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Types as T
-- import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, Node)
import Toolkit.Hydra2.Family.Feed.FCallGlslFunction (Node, State, _out_out) as FCallGlslFunction
-- import Toolkit.Hydra2 (State) as Hydra

type ListKey = List <^> "call-fn-list"



-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> FCallGlslFunction.Node m -> BlessedOp FCallGlslFunction.State m -- FIXME: why it doesn't work with `sendOut` ??
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
render nodeBoxKey node = do
    let
        (rootFnListKey :: ListKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        listKey = NK.append nodeBoxKey rootFnListKey
        glslFnList =
            B.listAnd listKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 14
                , Box.height $ Dimension.px 5
                , Box.scrollable true
                , List.items $ (T.render <<< T.glslFnItem) <$> Glsl.knownFns
                , List.mouse true
                , List.keys true
                , Box.tags true
                , Style.glslFnList
                , Core.on List.Select
                    \_ _ -> do
                        selected <- List.selected ~< listKey
                        {-
                        content <- TextArea.value ~< textBoxKey
                        let mbValues = T.findValues content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbValues of
                            Just values -> Node.sendOut node FArray._out_out values
                            Nothing -> pure unit
                        -}
                        pure unit
                ]
                [  ]
                $ \_ ->
                    pure unit
    nodeBoxKey >~ Node.append glslFnList




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit