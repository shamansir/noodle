module Cli.Components.NodeBox.OutletIndicator where

import Prelude


import Effect (Effect)

import Data.Text.Output.Blessed (singleLine) as T
import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option (content, height, left, top, width, tags) as Box
import Blessed.UI.Base.Element.PropertySet (setLeft, setTop) as Element

import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.UI.Base.Element.Method (hide, show, setFront) as Element

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style

import Noodle.Ui.Cli.Tagging (outletHover, outletSelect) as T


data Status
    = WaitConnection
    | Hover
    | Off


contentFor :: Status -> String
contentFor Off = ""
contentFor WaitConnection = T.singleLine $ T.outletSelect
contentFor Hover = T.singleLine $ T.outletHover


component ∷ forall state. Core.Blessed state
component =
    B.buttonAnd Key.outletIndicator
        [ Box.content $ contentFor Off
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Style.indicator
        ]
        []
        \_ -> do
            Key.outletIndicator >~ Element.hide


-- TODO: move to the corresponding output on hover
-- TODO: fix the indicator when output is selected for the link creation (and hide when the output is reset)

move :: forall state m. MonadThrow Error m => { x :: Int, y :: Int } -> BlessedOp state m
move { x, y } = do
    Key.outletIndicator >~ Element.setLeft $ Offset.px x
    Key.outletIndicator >~ Element.setTop  $ Offset.px y


updateStatus :: forall state m. Status -> BlessedOp state m
updateStatus status = do
    Key.outletIndicator >~ Element.setFront
    Key.outletIndicator >~ Box.setContent $ contentFor status
    Key.outletIndicator >~ Element.show


hide :: forall state m. BlessedOp state m
hide =
    Key.outletIndicator >~ Element.hide
