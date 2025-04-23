module Web.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Type.Proxy (Proxy(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS
-- import Halogen.Svg.Elements.None as HS
import Halogen.VDom.Driver (runUI)

import HydraTk.Toolkit (toolkit) as Hydra
import HydraTk.Patch (PState, init) as Hydra.Patch

import Web.Class.WebRenderer (ConstantShift)
import Web.Components.AppScreen (component) as AppScreen


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (H.hoist liftEffect $ AppScreen.component (Proxy :: _ ConstantShift) (Proxy :: _ Hydra.Patch.PState) Hydra.toolkit) unit body