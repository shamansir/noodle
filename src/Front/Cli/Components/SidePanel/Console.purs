module Cli.Components.SidePanel.Console where

import Prelude


import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State, withPanels)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle)


sidePanel :: forall tk p fs repr. SidePanel "console" (State tk p fs repr Effect) Boolean
sidePanel =
    { title : "console"
    , char : const 'L'
    , isOn : identity
    , panelKey : Key.consoleBox
    , buttonKey : Key.consoleButton
    , init : false /\ []
    , next : _.panels >>> load Console
    , onToggle : withPanels $ toggle Console
    }