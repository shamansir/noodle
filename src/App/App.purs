module App where

import Prelude

import Effect (Effect)

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
    }


run :: forall d. App d -> Effect Unit
run { style, flow, toolkit, network } = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI NetworkC.component { nw : network, toolkit, style, flow } body