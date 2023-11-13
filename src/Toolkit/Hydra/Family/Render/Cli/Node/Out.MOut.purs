module Tookit.Hydra.Family.Render.Cli.Out.FOut where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Control.Monad.State as State

import Type.Proxy (Proxy)

import Cli.Keys (NodeBoxKey)

import Data.Maybe (Maybe(..))

import Signal (Signal, (~>))
import Signal.Extra (runSignal)

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (Button)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift, impair1) as Blessed

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Boxes.Box.Method (setContent) as Box
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
import Blessed.Tagger as T

import Noodle.Node (sendInM, subscribeInput) as Node

import Tookit.Hydra.Types as H


-- import Noodle.Node (Node)

import Tookit.Hydra.Family.Out.FOut (Inputs, Outputs, State, Node, _in_target) as FOut

import Cli.Style as Style
import Cli.Tagging as T


type OutputButtonKey = Button <^> "output-button"


render :: forall m. MonadEffect m => MonadRec m => NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m -- FIXME: why it doesn't work with `sendOut` ??
render nodeBoxKey node = do
    let
        (rootOutputButtonKey :: OutputButtonKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        output0ButtonKey = NK.append nodeBoxKey rootOutputButtonKey
        output1ButtonKey = NK.next output0ButtonKey
        output2ButtonKey = NK.next output1ButtonKey
        output3ButtonKey = NK.next output2ButtonKey
        outputButton index output buttonKey =
            -- TODO: select if current
            B.button buttonKey
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px $ index * 2 --  index * 3
                , Box.width $ Dimension.px 2
                , Box.height $ Dimension.px 1
                , Box.content $ show index -- "O" <> show index -- $ show output
                , Box.tags true
                , Style.inputBox
                , Button.mouse true
                , Core.on Button.Press
                    \_ _ -> do
                        Node.sendInM node FOut._in_target output
                        State.put output
                ]
                [  ]
        outputButton0 = outputButton 0 H.Output0 output0ButtonKey
        outputButton1 = outputButton 1 H.Output1 output1ButtonKey
        outputButton2 = outputButton 2 H.Output2 output2ButtonKey
        outputButton3 = outputButton 3 H.Output3 output3ButtonKey
    nodeBoxKey >~ Node.append outputButton0
    nodeBoxKey >~ Node.append outputButton1
    nodeBoxKey >~ Node.append outputButton2
    nodeBoxKey >~ Node.append outputButton3
    renderer <- Blessed.impair1
                    $ \repr ->
                        case repr of
                            H.Output0 -> do
                                output0ButtonKey >~ Box.setContent $ T.render $ T.selected $ show 0
                                output1ButtonKey >~ Box.setContent $ show 1
                                output2ButtonKey >~ Box.setContent $ show 2
                                output3ButtonKey >~ Box.setContent $ show 3
                            H.Output1 -> do
                                output0ButtonKey >~ Box.setContent $ show 0
                                output1ButtonKey >~ Box.setContent $ T.render $ T.selected $ show 1
                                output2ButtonKey >~ Box.setContent $ show 2
                                output3ButtonKey >~ Box.setContent $ show 3
                            H.Output2 -> do
                                output0ButtonKey >~ Box.setContent $ show 0
                                output1ButtonKey >~ Box.setContent $ show 1
                                output2ButtonKey >~ Box.setContent $ T.render $ T.selected $ show 2
                                output3ButtonKey >~ Box.setContent $ show 3
                            H.Output3 -> do
                                output0ButtonKey >~ Box.setContent $  show 0
                                output1ButtonKey >~ Box.setContent $ show 1
                                output2ButtonKey >~ Box.setContent $ show 2
                                output3ButtonKey >~ Box.setContent $ T.render $ T.selected $ show 3
                            _ -> pure unit

    Blessed.lift $ runSignal $ Node.subscribeInput _.target node ~> renderer