module Toolkit.Hydra2.Family.Render.Cli.Out.FOut where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State as State

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody)

import Cli.Keys (NodeBoxKey)

import Data.Maybe (Maybe(..))

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (Button)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button


import Blessed.UI.Boxes.Box.Option as Box

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))

import Noodle.Node2 (sendIn) as Node

import Toolkit.Hydra2.Types as H


-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node, _in_target) as FOut

import Cli.Style as Style


type OutputButtonKey = Button <^> "output-button"


render :: forall m. NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m -- FIXME: why it doesn't work with `sendOut` ??
render nodeBoxKey node = do
    let
        (rootOutputButtonKey :: OutputButtonKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        firstOutputButtonKey = NK.append nodeBoxKey rootOutputButtonKey
        outputButton index output buttonKey =
            -- TODO: select if current
            B.button buttonKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px $ index * 2 --  index * 3
                , Box.width $ Dimension.px 1
                , Box.height $ Dimension.px 1
                , Box.content $ show index -- "O" <> show index -- $ show output
                , Style.inputBox
                , Button.mouse true
                , Core.on Button.Press
                    \_ _ -> do
                        Node.sendIn node FOut._in_target output
                        State.put output
                ]
                [  ]
        outputButton0 = outputButton 0 H.Output0 firstOutputButtonKey
        outputButton1 = outputButton 1 H.Output1 $ NK.next firstOutputButtonKey
        outputButton2 = outputButton 2 H.Output2 $ NK.next $ NK.next firstOutputButtonKey
        outputButton3 = outputButton 3 H.Output3 $ NK.next $ NK.next $ NK.next firstOutputButtonKey
    nodeBoxKey >~ Node.append outputButton0
    nodeBoxKey >~ Node.append outputButton1
    nodeBoxKey >~ Node.append outputButton2
    nodeBoxKey >~ Node.append outputButton3