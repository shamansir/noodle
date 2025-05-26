module Web.Components.SidePanel.CommandLog where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))


import Web.Components.SidePanel (SidePanel)

import Noodle.Text.NdfFile (NdfFile)

import Web.Components.AppScreen.State as CState


panelId = Proxy :: _ "command-log"


sidePanel :: SidePanel "command-log" NdfFile Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , next : \ndfFile -> pure $ true /\ CState.formatHistory ndfFile
    , onToggle : identity
    }