module Test.Main where

import Prelude (Unit, ($))

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Nodes (spec) as Nodes


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle"
    Nodes.spec