module Web.Components.SidePanel.Console where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Newtype (class Newtype, wrap, unwrap)

import Web.Components.SidePanel (SidePanel)

import Noodle.Ui.Tagging as T


newtype LogLine = LogLine String


derive instance Newtype LogLine _


panelId = Proxy :: _ "console"


sidePanel :: SidePanel "console" (Array LogLine) Unit
sidePanel =
    { title : "console"
    , char : const 'L'
    , next : \logLines -> pure $ T.s <$> unwrap <$> logLines
    , value : const unit
    }