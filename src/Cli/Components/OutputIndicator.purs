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

import Blessed.UI.Boxes.Box.Method (setContent) as Box

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


component ∷ Core.Blessed State
component =
    B.button Key.outputIndicator
        [ Box.content $ contentFor Off
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Style.indicator
        ]
        []


-- TODO: move to the corresponding output on hover
-- TODO: fix the indicator when output is selected for the link creation (and hide when the output is reset)


updateStatus :: Status -> BlessedOp State Effect
updateStatus status =
    Key.outputIndicator >~ Box.setContent $ contentFor status