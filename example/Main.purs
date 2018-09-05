module Example.Main where

import Prelude

import Effect (Effect)

import Spork.Html (Html)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp

import Rpd (RpdError, Rpd, Network, emptyNetwork)
import Rpd.Render (update, once, Renderer, proxy') as Render
import Rpd.Render (Message(..)) as Ui
import Rpd.Render.Terminal (view) as TerminalRenderer
import Rpd.Render.Terminal (terminalRenderer)


type Model d = Rpd d
type Action d = Ui.Message d


sporkRenderer :: forall d. Render.Renderer d (Html (Action d))
sporkRenderer =
  terminalRenderer
    # Render.proxy'
      (const $ H.div [] [])
      inject
      (H.div [] [])


inject :: forall d. (Ui.Message d -> Effect Unit) -> String -> Html (Action d)
inject pushMsg strView =
  H.div [] []


update = flip Render.update


render ∷ forall d. Model d → Html (Action d)
render rpd =
  let effHtml = Render.once sporkRenderer rpd
  in H.div []
    [ H.button
        [ H.onClick (H.always_ Ui.Bang) ]
        [ H.text "Hit me" ]
    ]

app ∷ forall d. PureApp (Model d) (Action d)
app = { update, render, init: pure emptyNetwork }


main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"
