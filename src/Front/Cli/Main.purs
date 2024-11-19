module Cli.Main where

import Prelude

import Effect (Effect)

import Cli.App (run) as App


main :: Effect Unit
main =
    App.run