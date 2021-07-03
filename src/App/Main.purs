module App.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


import Noodle.Network as Network
import App.Component.Network as NetworkC


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI NetworkC.component { nw : Network.empty } body