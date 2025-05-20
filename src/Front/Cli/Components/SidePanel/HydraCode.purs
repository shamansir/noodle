module Cli.Components.SidePanel.HydraCode where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.State (isPanelOn, togglePanel) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Front.Shared.Panels (Which(..)) as P


sidePanel :: forall tk p fs sr cr. SidePanel "hydra-code" (State _ tk p fs sr cr Effect) Boolean
sidePanel =
    { title : "hydra"
    , char : const 'H'
    , isOn : identity
    , panelKey : Key.hydraCodeBox
    , buttonKey : Key.hydraCodeButton
    , next : \s -> pure $ CState.isPanelOn P.WsServer s /\ []
    , onToggle : CState.togglePanel P.WsServer
    }
