module Cli.Components.WsStatusButton where

import Prelude

import Effect (Effect)

import Data.Either (Either(..))

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

import Cli.Tagging (buttonConnection) as T


data Status
    = Off
    | Waiting
    | Connected Int


contentFor :: Status -> String
contentFor Off = T.render $ T.buttonConnection $ Left "-"
contentFor Waiting = T.render $ T.buttonConnection $ Right 0
contentFor (Connected n) = T.render $ T.buttonConnection $ Right n


component ∷ Core.Blessed State
component =
    B.button Key.wsStatusButton
        [ Box.content $ contentFor Off
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 11
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Style.menuButton
        ]
        []


updateStatus :: Status -> BlessedOp State Effect
updateStatus status =
    Key.wsStatusButton >~ Box.setContent $ contentFor status