module Cli.Components.SidePanel.WsServerStatus where


import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.State (togglePanel, isPanelOn) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..)) as P


data Status
    = Off
    | Waiting
    | Connected Int


sidePanel :: forall tk p fs sr cr. SidePanel "ws-status" (State tk p fs sr cr Effect) Boolean
sidePanel =
    { title : "server"
    , char : const 'W'
    , isOn : identity
    , panelKey : Key.wsStatusBox
    , buttonKey : Key.wsStatusButton
    , next : \s -> CState.isPanelOn P.WsServer s /\ []
    , onToggle : CState.togglePanel P.WsServer
    }