module Tookit.Hydra.Family.Render.Cli.Feed.FArray where

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

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
import Blessed.UI.Forms.TextArea.Event as TextArea
import Blessed.UI.Forms.TextArea.Property as TextArea

import Noodle.Node (sendOutM) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style

-- import Noodle.Node (Node)

import Tookit.Hydra.Types as T
-- import Tookit.Hydra.Family.Feed.FNumber (Inputs, Outputs, Node)
import Tookit.Hydra.Family.Feed.FArray (Node, State, _out_out) as FArray
-- import Tookit.Hydra (State) as Hydra
import Tookit.Hydra.Repr.Wrap.Parser (findValues) as T

type TextBoxKey = TextBox <^> "array-text-box"



-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
render nodeBoxKey node = do
    let
        (rootTextBoxKey :: TextBoxKey) = NK.first -- FIXME, find the next one from state or as passed to the node
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
                        content <- TextArea.value ~< textBoxKey
                        let mbValues = T.findValues content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbValues of
                            Just values -> Node.sendOutM node FArray._out_out values
                            Nothing -> pure unit
                        pure unit
                -- , Core.on Element.KeyPress
                --     \_ _ -> do
                --         pure unit
                ]
                [  ]
    nodeBoxKey >~ Node.append innerText




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit