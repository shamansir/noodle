module Cli.Components.CommandLogButton where


import Prelude

import Data.Text.Output (render) as T

import Control.Monad.State as State

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option (content, height, left, top, width, tags) as Box

import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys as Key
import Cli.State (State)
import Cli.State as State
import Cli.Style as Style

import Cli.Components.CommandLogBox as CommandLogBox

import Cli.Tagging as T


component ∷ Core.Blessed State
component =
    B.button Key.commandLogButton
        [ Box.content $ T.render $ T.buttonToggle "C" false
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 5
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        , Style.menuButton
        , Core.on Button.Press
            \_ _ -> do
                State.modify_ State.toggleCommandBox
                Key.commandLogBox >~ Element.toggle
                state <- State.get
                Key.commandLogButton >~ Box.setContent $ T.render $ T.buttonToggle "C" state.commandBoxOn
                CommandLogBox.refresh
                Key.mainScreen >~ Screen.render
        {-
        , Core.on Element.MouseOver
            \_ _ -> do
                liftEffect $ Console.log "over"
        , Core.on Element.MouseOut
            \_ _ -> do
                liftEffect $ Console.log "out"
        -}
        ]
        []