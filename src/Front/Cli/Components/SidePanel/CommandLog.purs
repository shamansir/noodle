module Cli.Components.SidePanel.CommandLog where

import Prelude

import Effect (Effect)

import Cli.State (State)
import Cli.Components.SidePanel (SidePanel)
import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key



sidePanel :: forall tk p fs repr. SidePanel "command-log" (State tk p fs repr Effect)
sidePanel =
    { char : { on : 'C', off : 'X' }
    , panelKey : Key.commandLogBox
    , buttonKey : Key.commandLogButton
    , init : []
    , next : const []
    , toggle : identity
    }