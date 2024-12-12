module Cli.Components.SidePanel.WsServerStatus where


import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Cli.State (State, withPanels)
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle)


data Status
    = Off
    | Waiting
    | Connected Int


sidePanel :: forall tk p fs repr. SidePanel "ws-status" (State tk p fs repr Effect) Boolean
sidePanel =
    { title : "server"
    , char : const 'W'
    , isOn : identity
    , panelKey : Key.wsStatusBox
    , buttonKey : Key.wsStatusButton
    , init : false /\ []
    , next : _.panels >>> load WsServer
    , onToggle : withPanels $ toggle WsServer
    }