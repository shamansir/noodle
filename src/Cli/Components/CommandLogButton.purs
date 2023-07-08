module Cli.Components.CommandLogButton where


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

import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.Palette as Palette
import Cli.State (State)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Style as Style

import Cli.Components.CommandLogBox as CommandLogBox

import Noodle.Network2 as Network
import Noodle.Patch4 as Patch

import Toolkit.Hydra2 as Hydra


component ∷ Core.Blessed State
component =
    B.button Key.addPatchButton
        [ Box.content "C"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 5
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                Key.commandLogBox >~ Element.toggle
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