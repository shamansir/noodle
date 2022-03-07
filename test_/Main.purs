module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Nodes (spec) as Nodes
import Test.Layouts (spec) as Layouts
import Test.Hydra.Compilation (spec) as HydraCompilation


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle"
    Nodes.spec
  describe "Layouts"
    Layouts.spec
  describe "Hydra" do
    describe "Compilation"
      HydraCompilation.spec