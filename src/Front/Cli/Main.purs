module Cli.Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Type.Proxy (Proxy(..))

import Cli.App (run) as App


import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (empty) as Toolkit


toolkit = Toolkit.empty $ Id.toolkitR "Empty"


main :: Effect Unit
main = do
    App.run (Proxy :: _ Unit) toolkit