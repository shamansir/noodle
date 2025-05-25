module Web.Components.SidePanel.CommandLog where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Newtype (class Newtype, wrap, unwrap)


import Web.Components.SidePanel (SidePanel)

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Command.Op (CommandOp)

import Web.Components.AppScreen.State as CState


sidePanel :: SidePanel "command-log" NdfFile Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , next : \ndfFile -> pure $ true /\ CState.formatHistory ndfFile
    , onToggle : identity
    }