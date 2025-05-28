module Web.Components.SidePanel.CommandLog where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))


import Web.Components.SidePanel (SidePanel)

import Noodle.Text.NdfFile (NdfFile)

import Web.Components.AppScreen.State as CState


panelId = Proxy :: _ "command-log"


sidePanel :: SidePanel "command-log" NdfFile Unit
sidePanel =
    { title : "history"
    , char : const '⏺'
    , next : \ndfFile -> pure $ CState.formatHistory ndfFile
    , value : const unit
    }