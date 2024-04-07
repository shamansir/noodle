module Cli.Components.HydraCodeButton where


import Prelude

import Control.Monad.State (get, modify_) as State

import Data.Text.Format as T
import Data.Text.Output (render) as T

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

import Cli.Components.HydraCodeBox as HydraCodeBox

import Cli.Tagging as T


component ∷ Core.Blessed State
component =
    B.button Key.hydraCodeButton
        [ Box.content $ T.render $ T.buttonToggle "H" false
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 7
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                State.modify_ State.toggleHydraCode
                Key.hydraCodeBox >~ Element.toggle
                state <- State.get
                Key.hydraCodeButton >~ Box.setContent $ T.render $ T.buttonToggle "H" state.hydraCodeOn
                HydraCodeBox.refresh
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
