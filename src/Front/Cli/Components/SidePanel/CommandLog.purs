module Cli.Components.SidePanel.CommandLog where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State, withPanels)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle)



sidePanel :: forall tk p fs repr. SidePanel "command-log" (State tk p fs repr Effect) Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , panelKey : Key.commandLogBox
    , buttonKey : Key.commandLogButton
    , init : false /\ []
    , next : _.panels >>> load Commands
    , onToggle : withPanels $ toggle Commands
    }