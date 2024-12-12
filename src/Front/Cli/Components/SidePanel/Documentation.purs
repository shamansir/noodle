module Cli.Components.SidePanel.Documentation where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key


sidePanel :: forall tk p fs repr. SidePanel "documentation" (State tk p fs repr Effect) Boolean
sidePanel =
    { title : "documentation"
    , char : const '☰'
    , isOn : identity
    , panelKey : Key.documentationBox
    , buttonKey : Key.documentationButton
    , init : false /\ []
    , next : const $ true /\ []
    , onToggle : identity
    }
