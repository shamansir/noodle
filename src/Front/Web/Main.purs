module Web.Main where

import Prelude

import Effect (Effect)
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
import HydraTk.Patch (init) as Hydra.Patch

import Front.Web.Components.MainScreen (component) as MainScreen

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (MainScreen.component Hydra.Patch.init Hydra.toolkit) unit body


data Action = Increment | Decrement

{-
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      , HH.text "No chance"
      , HH.text "Test Ok? Foo. How come my rebuild? Maybe now? And now?"
      , HS.svg [ HSA.width 100.0, HSA.height 100.0 ] []
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1
-}