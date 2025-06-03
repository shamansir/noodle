module Web.Components.SidePanel.HydraCode where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Text.Format (s) as T
import Data.Maybe (Maybe, fromMaybe)

import Web.Components.SidePanel (SidePanel)

import HydraTk.Lang.Program (Program, printToJavaScript) as Hydra


panelId = Proxy :: _ "hydra"


sidePanel :: SidePanel "hydra" (Maybe Hydra.Program) Unit
sidePanel =
    { title : "hydra"
    , char : const 'H'
    , value : const unit
    , next : pure <<< pure <<< T.s <<< fromMaybe "..." <<< map Hydra.printToJavaScript
    }