module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Fn (spec) as Fn
import Test.UniqueHash (spec) as UniqueHash


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "UniqueHash"
    UniqueHash.spec
  describe "Fn"
    Fn.spec