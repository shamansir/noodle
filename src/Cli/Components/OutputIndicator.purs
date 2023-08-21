module Cli.Components.OutputIndicator where

import Prelude


import Effect (Effect)

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

import Blessed.Tagger (render) as T

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style

import Cli.Tagging (outputHover, outputSelect) as T


data Status
    = WaitConnection
    | Hover
    | Off


contentFor :: Status -> String
contentFor Off = ""
contentFor WaitConnection = T.render $ T.outputSelect
contentFor Hover = T.render $ T.outputHover


component ∷ forall state. Core.Blessed state
component =
    B.buttonAnd Key.outputIndicator
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
            Key.outputIndicator >~ Element.hide


-- TODO: move to the corresponding output on hover
-- TODO: fix the indicator when output is selected for the link creation (and hide when the output is reset)

move :: forall state. { x :: Int, y :: Int } -> BlessedOp state Effect
move { x, y } = do
    Key.outputIndicator >~ Element.setLeft $ Offset.px x
    Key.outputIndicator >~ Element.setTop $ Offset.px y


updateStatus :: forall state. Status -> BlessedOp state Effect
updateStatus status = do
    Key.outputIndicator >~ Element.setFront
    Key.outputIndicator >~ Box.setContent $ contentFor status
    Key.outputIndicator >~ Element.show


hide :: forall state. BlessedOp state Effect
hide =
    Key.outputIndicator >~ Element.hide