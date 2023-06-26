module Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber where

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
import Blessed.Internal.BlessedSubj (TextBox)
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

import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
import Blessed.UI.Forms.TextArea.Event as TextArea
import Blessed.UI.Boxes.Box.Property as Element

import Noodle.Node2 as Node

import Cli.Components.NodeBox.HasBody (class HasBody)
import Cli.Keys (NodeBoxKey)
import Cli.Style as Style

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Types as T
import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, Node)
import Toolkit.Hydra2.Family.Feed.FNumber (State, _out_out) as FNumber
-- import Toolkit.Hydra2 (State) as Hydra

type TextBoxKey = TextBox <^> "number-tex-box"




-- instance HasBody "number" State Inputs Outputs m where
--     run :: NodeBoxKey -> Node m -> Maybe (BlessedOp State m)
--     run _ _ = Nothing


-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> Node m -> BlessedOp FNumber.State m -- FIXME: why it doesn't work with `sendOut` ??
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
render nodeBoxKey node = do
    let
        (rootTextBoxKey :: TextBoxKey) = NK.first -- FIXME
        textBoxKey = NK.append nodeBoxKey rootTextBoxKey
        innerText =
            B.textBox textBoxKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.percents 85.0
                , Box.height $ Dimension.px 1
                , Style.inputBox
                , TextArea.mouse true
                , TextArea.inputOnFocus true
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- Element.content ~< textBoxKey
                        let mbNumber = Number.fromString content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbNumber of
                            Just number -> Node.sendOut node FNumber._out_out $ T.Number number
                            -- Just number -> Node.sendOut node FNumber._out_out $ T.Number number
                            Nothing -> pure unit
                        pure unit
                ]
                [  ]
    nodeBoxKey >~ Node.append innerText




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit