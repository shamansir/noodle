module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (elementToNode, Node)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.VirtualDOM (patch, VNode, text, prop, h, with, EventListener(On))
import Data.VirtualDOM.DOM (api)
import Signal (sampleOn, runSignal, (~>), foldp, Signal)
import Signal.Channel (subscribe, send, CHANNEL, channel, Channel)
import Signal.DOM (animationFrame)

type State = Int

initialState :: State
initialState = 0

data Action = Noop | Pred | Succ

nat :: Int → String
nat 0 = "Z"
nat n = "S " <> nat (n - 1)

render :: ∀ e l v. Channel Action → State → VNode (channel :: CHANNEL | e) l v
render actions state = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: rgb(" <> show (min (state * 8) 256) <> ",0,0)")]) [text $ "Number " <> nat state]
  , with (h "button" (prop []) [text "pred"]) [On "click" \_ -> send actions Pred]
  , with (h "button" (prop []) [text "succ"]) [On "click" \_ -> send actions Succ]
  ]

update :: Action → State → State
update action state = case action of
  Noop → state
  Pred → max (state - 1) 0
  Succ → state + 1

app :: ∀ e. Signal State → Channel Action → Node → Eff (dom :: DOM, channel :: CHANNEL, timer :: TIMER | e) Unit
app stateS actions target = do
  tick ← animationFrame
  runSignal $ (input (sampleOn tick stateS)) ~> write
  where
    input state = foldp go (Tuple Nothing Nothing) state
    go state (Tuple _ prev) = Tuple prev (Just $ render actions state)
    write (Tuple prev next) = patch api target prev next

main :: ∀ e. Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, timer :: TIMER | e) Unit
main = do
  doc ← window >>= document >>= htmlDocumentToParentNode >>> pure
  target ← querySelector (QuerySelector "#content") doc >>= map elementToNode >>> pure
  actions ← channel Noop
  let state = foldp update initialState $ subscribe actions
  maybe (log "No div#content found!") (app state actions) target
