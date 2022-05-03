module App (App, App', run) where

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


type App d = App' Unit Unit d


--type App' patch_action patch_state d = AppC.Input patch_action patch_state d Aff
type App' patch_state node_state d = AppC.Input patch_state node_state d


run :: forall patch_state node_state d. App' patch_state node_state d -> Effect Unit
run app = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI
        AppC.component
        app
        body