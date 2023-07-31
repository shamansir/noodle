module Cli.Components.WsStatusButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Effect.Console (log) as Console

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys as Key
import Cli.Palette as Palette
import Cli.State (State)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Components.PatchesListbar as PatchesListbar
import Cli.Style as Style

import Noodle.Network2 as Network
import Noodle.Patch4 as Patch

import Toolkit.Hydra2 as Hydra


data Status
    = Off
    | Waiting
    | Connected Int


contentFor :: Status -> String
contentFor Off = "-"
contentFor Waiting = "0"
contentFor (Connected n) = show n


component ∷ Core.Blessed State
component =
    B.button Key.wsStatusButton
        [ Box.content $ contentFor Off
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 9
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Style.menuButton
        ]
        []


updateStatus :: Status -> BlessedOp State Effect
updateStatus status =
    Key.wsStatusButton >~ Box.setContent $ contentFor status