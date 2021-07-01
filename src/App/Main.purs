module App.Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type State = Int

data Action = Increment | Decrement

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.text (show state)
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1