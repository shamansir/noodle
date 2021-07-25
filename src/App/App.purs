module App where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


import Noodle.Network (Network)
import Noodle.Toolkit (Toolkit)

import App.Component.Network as NetworkC
import App.Style (Style, NodeFlow)


type App d =
    { style :: Style
    , flow :: NodeFlow
    , toolkit :: Toolkit d
    , network :: Network d
    , currentPatch :: Maybe String
    }


run :: forall d. App d -> Effect Unit
run app = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI NetworkC.component app body