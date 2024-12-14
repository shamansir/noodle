module Cli.Components.SidePanel.HydraCode where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State, withPanels)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle)


sidePanel :: forall tk p fs sr cr. SidePanel "hydra-code" (State tk p fs sr cr Effect) Boolean
sidePanel =
    { title : "hydra"
    , char : const 'H'
    , isOn : identity
    , panelKey : Key.hydraCodeBox
    , buttonKey : Key.hydraCodeButton
    , init : false /\ []
    , next : _.panels >>> load Console
    , onToggle : withPanels $ toggle Console
    }
