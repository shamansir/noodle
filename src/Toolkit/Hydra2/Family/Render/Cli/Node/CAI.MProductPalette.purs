module Toolkit.Hydra2.Family.Render.Cli.Node.CAI.FProductPalette where

import Prelude

import Debug as Debug

import Type.Proxy (Proxy)
import Data.Number as Number

import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Data.Maybe (Maybe(..))

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (Button)
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
import Blessed.UI.Boxes.Box.Option (content, height, left, top, width, tags) as Box
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Method (setContent) as Box


import Noodle.Node2 (sendInM) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style

import CompArts.Product as CAI

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Types as T
-- import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, Node)
import Toolkit.Hydra2.Family.CAI.FProductPalette (Node, State, _in_product) as FProductPalette
-- import Toolkit.Hydra2 (State) as Hydra

type ButtonKey = Button <^> "product-palette-text-box"



render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> FProductPalette.Node m -> BlessedOp FProductPalette.State m
render nodeBoxKey node = do
    products <- State.get
    let _ = Debug.spy "p" products
    let
        (rootButtonKey :: ButtonKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        buttonKey = NK.append nodeBoxKey rootButtonKey
        theButton =
            B.button buttonKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.percents 85.0
                , Box.height $ Dimension.px 1
                , Style.inputBox
                , Button.mouse true
                , Box.content $ show $ CAI.count products
                {-
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- TextArea.value ~< textBoxKey
                        let mbNumber = Number.fromString content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbNumber of
                            Just number -> Node.sendInM node FProductPalette._in_product $ T.Number number
                                -- Just number -> Node.sendOut node FNumber._out_out $ T.Number number
                            Nothing -> pure unit
                        pure unit
                 -}
                ]
                [  ]
    nodeBoxKey >~ Node.append theButton




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit