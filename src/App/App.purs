module App (App, run) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


import Noodle.Network (Network)
import Noodle.Toolkit (Toolkit)
import Noodle.Patch as Patch

import App.Component.App as AppC
import App.Style (Style, NodeFlow)
import App.Toolkit.UI (UI)


type App s d = AppC.Input Aff s d


run :: forall s d. App s d -> Effect Unit
run app = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI
        AppC.component
        app
        body