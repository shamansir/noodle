module Example.Main where

import Prelude

import Effect (Effect)
import Data.Const (Const)

import Spork.Html (Html)
import Spork.Html as H
import Spork.App (App)
import Spork.App as App

import Rpd (RpdError, Rpd, Network, emptyNetwork)
import Rpd (init) as Rpd
import Rpd.Render (update, once, Renderer, proxy') as Render
import Rpd.Render (Message(..)) as Ui
import Rpd.Render.Terminal (view) as TerminalRenderer
import Rpd.Render.Terminal (terminalRenderer)
import Spork.Interpreter (never)


type Model d = Rpd (Network d)
type Action d = Ui.Message d


sporkRenderer :: forall d. Render.Renderer d (Html (Action d))
sporkRenderer =
  terminalRenderer
    # Render.proxy'
      (const $ H.div [] [])
      inject
      (H.div [] [])
  where
    inject :: (Ui.Message d -> Effect Unit) -> String -> Html (Action d)
    inject pushMsg strView =
      H.div [] []


render ∷ forall d. Model d → Html (Action d)
render rpd =
  let (effHtml :: Effect (Html (Action d))) = Render.once sporkRenderer rpd
  -- TerminalRenderer.view
  in H.div []
    [ H.button
        [ H.onClick (H.always_ Ui.Bang) ]
        [ H.text "Hit me" ]
    ]

update :: forall d. Model d -> Action d -> App.Transition Effect (Model d) (Action d)
update model action =
  { model: model >>= Render.update action
  , effects: mempty
  }


init :: forall d. App.Transition Effect (Model d) (Action d)
init =
  { model: Rpd.init "a"
  , effects: mempty
  }

app ∷ forall d. App Effect (Const Void) (Model d) (Action d)
app =
  { render: render
  , update: update
  , subs: const mempty
  , init: init
  }


-- main ∷ Effect Unit
-- main = void $ App.makeWithSelector app "#app"
