module Web.Components.SidePanel.Console where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Newtype (class Newtype, wrap, unwrap)

import Web.Components.SidePanel (SidePanel)

import Noodle.Ui.Tagging as T


newtype LogLine = LogLine String


derive instance Newtype LogLine _


sidePanel :: SidePanel "console" (Array LogLine) Boolean
sidePanel =
    { title : "console"
    , char : const 'L'
    , isOn : identity
    , next : \logLines -> pure $ true /\ (T.s <$> unwrap <$> logLines)  -- const $ pure $ true /\ [ T.s "Command", T.s "Log" ]
    , onToggle : identity
    }