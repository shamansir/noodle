module Cli.Components.SidePanel.CommandLog where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key



sidePanel :: forall tk p fs repr. SidePanel "command-log" (State tk p fs repr Effect) Boolean
sidePanel =
    { char : const 'C'
    , isOn : identity
    , panelKey : Key.commandLogBox
    , buttonKey : Key.commandLogButton
    , init : false /\ []
    , next : const $ true /\ []
    , onButton : identity
    }