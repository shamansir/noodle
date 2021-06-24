module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Node (Node(..))


main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  let
    fn [a] = pure a
    fn [b] = pure b
    fn [a, b] = pure $ a + b
    fn [] = pure $ 0
    fn [a, b, _] = pure $ a + b
    node = Node fn
  log ""
