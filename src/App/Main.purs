module App.Main where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


import Noodle.Network as Network
import Noodle.Patch as Patch

import App.Component.Network as NetworkC
import App.Example.Toolkit (toolkit)

import App.Style (Style)
import App.Style.Quartz as Quartz


style :: Style
style = Quartz.style


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI NetworkC.component { nw, toolkit, style } body
  where nw = Network.empty # Network.addPatch ( "base" /\ Patch.empty )
