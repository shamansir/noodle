module Web.Components.SidePanel.CommandLog where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))


import Web.Components.SidePanel (SidePanel)

import Noodle.Text.NdfFile (NdfFile)

import Web.Components.AppScreen.State as CState


sidePanel :: SidePanel "command-log" NdfFile Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , next : \ndfFile -> pure $ true /\ CState.formatHistory ndfFile
    , onToggle : identity
    }