module Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber where

import Prelude

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody)

import Cli.Keys (NodeBoxKey)

import Data.Maybe (Maybe(..))

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (TextBox)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Forms.TextArea.Option as TextArea

import Blessed.Internal.NodeKey as NodeKey
import Cli.Style as Style

import Blessed.Core.Border as Border
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))


-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, Node)

import Toolkit.Hydra2.Family.Feed.FNumber (State) as FNumber
-- import Toolkit.Hydra2 (State) as Hydra

type TextBoxKey = TextBox <^> "number-tex-box"




-- instance HasBody "number" State Inputs Outputs m where
--     run :: NodeBoxKey -> Node m -> Maybe (BlessedOp State m)
--     run _ _ = Nothing


-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
render nodeBoxKey _ = do
    let
        (rootTextBoxKey :: TextBoxKey) = NK.first -- FIXME
        textBoxKey = NK.append nodeBoxKey rootTextBoxKey
        innerText =
            B.textBox textBoxKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.content "text"
                , Box.width $ Dimension.percents 85.0
                , Box.height $ Dimension.px 1
                , Style.inputBox
                , TextArea.mouse true
                , TextArea.inputOnFocus true
                ]
                [ ]
    nodeBoxKey >~ Node.append innerText




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit