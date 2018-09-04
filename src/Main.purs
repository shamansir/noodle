module Main where

import Prelude

import Effect (Effect)

import Spork.Html (Html)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp

type Model = {}

data Action = Bang

update ∷ Model → Action → Model
update model = case _ of
  Bang → model

render ∷ Model → Html Action
render i =
  H.div []
    [ H.button
        [ H.onClick (H.always_ Bang) ]
        [ H.text "+" ]
    ]


app ∷ PureApp Model Action
app = { update, render, init: {} }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"
