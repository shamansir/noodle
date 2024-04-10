module Toolkit.Hydra.Family.Render.Cli.Display.FInfo where

import Prelude

import Type.Proxy (Proxy)
import Data.Number as Number
import Data.Text.Output.Blessed (render) as T

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Console as Console

import Data.Maybe (Maybe(..))

import Signal (Signal, (~>))
import Signal.Extra (runSignal, class RunInSignal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (TextBox)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift, impair1) as Blessed
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
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
import Blessed.UI.Forms.TextArea.Event as TextArea
import Blessed.UI.Boxes.Box.Property as Element

import Noodle.Node (sendOut, subscribeInput) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style
import Cli.Tagging as T

-- import Noodle.Node (Node)

import Toolkit.Hydra.Types as T
-- import Toolkit.Hydra.Family.Feed.FNumber (Inputs, Outputs, Node)
import Toolkit.Hydra.Family.Display.FInfo (Node, State, _in_in) as FInfo
-- import Toolkit.Hydra (State) as Hydra


type TextBoxKey = TextBox <^> "info-text-box"


render :: forall m. RunInSignal m => MonadRec m => MonadEffect m => NodeBoxKey -> FInfo.Node m -> BlessedOp FInfo.State m
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
                , Box.tags true
                , Style.inputBox
                -- , TextArea.mouse true
                ]
                [  ]
    nodeBoxKey >~ Node.append innerText
    renderer <- Blessed.impair1 $ \repr -> textBoxKey >~ Box.setContent $ T.render $ T.infoNode repr
    Blessed.lift $ runSignal $ Node.subscribeInput _.in node ~> renderer