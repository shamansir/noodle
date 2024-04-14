module Cli.Components.FullInfoButton where

import Prelude

import Data.Text.Output.Blessed (singleLine) as T

import Control.Monad.State (get, modify_) as State

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
import Blessed.UI.Base.Element.Method (toggle, show) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys as Key
import Cli.State (State)
import Cli.State as State
import Cli.Style as Style

import Cli.Components.FullInfoBox as FullInfoBox

import Cli.Tagging as T


component ∷ Core.Blessed State
component =
    B.button Key.fullInfoButton
        [ Box.content $ T.singleLine $ T.buttonToggle "F" false
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 9
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                State.modify_ State.toggleFullInfo
                Key.fullInfoBox >~ Element.toggle
                state <- State.get
                Key.fullInfoButton >~ Box.setContent $ T.singleLine $ T.buttonToggle "F" state.fullInfoOn
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
