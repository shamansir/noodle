module Cli.Components.SidePanel.WebSocketStatus where


import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.State (togglePanel, isPanelOn) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key

import Front.Shared.Panels (Which(..)) as P


sidePanel :: forall tk p fs sr cr. SidePanel "ws-status" (State _ tk p fs sr cr Effect) Boolean -- FIXME: use `Status` here
sidePanel =
    { title : "server"
    , char : const 'W'
    , isOn : identity
    , panelKey : Key.wsStatusBox
    , buttonKey : Key.wsStatusButton
    , next : \s -> pure $ CState.isPanelOn P.WSStatus s /\ []
    , onToggle : CState.togglePanel P.WSStatus
    }