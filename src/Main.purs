module Main where

import Prelude


import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)

import Cli.App as Cli

import Blessed as B
import Blessed.UI.Node (Node)
import Blessed.UI.Screen as Screen


main :: Effect Unit
main = do
  Cli.run
    (B.screen "main"
      [ Screen.title "foo" ]
      []
    )
