module Web.App2 (App, run) where

import Prelude

import Effect (Effect)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Web.App.Component.App3 as AppC


type App gstate nodes instances =
    AppC.Input gstate nodes instances


run
    :: forall gstate ( nodes :: Row Type ) ( instances :: Row Type)
     . {- Record.Keys rln ⇒ RL.RowToList instances rln
    => Record.Keys rln ⇒ RL.RowToList nodes rln
    => -} App gstate nodes instances
    -> Effect Unit
run app = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI
        AppC.component
        app
        body