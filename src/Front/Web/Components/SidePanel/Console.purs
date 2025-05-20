module Web.Components.SidePanel.Console where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T

import Web.Components.SidePanel (SidePanel)
import Web.Components.AppScreen.State (State)

import Noodle.Ui.Tagging as T


sidePanel :: forall tk ps fs sr cr m. SidePanel "console" (State tk ps fs sr cr m) Boolean
sidePanel =
    { title : "console"
    , char : const 'L'
    , isOn : identity
    , next : const $ pure $ true /\ [ T.s "Command", T.s "Log" ]
    , onToggle : identity
    }