module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Nodes (spec) as Nodes
import Test.Fn (spec) as Fn
import Test.Toolkit (spec) as Toolkit
import Test.Toolkit2 (spec) as Toolkit2
import Test.Flex (spec) as Flex


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Fn"
    Fn.spec
  describe "Nodes"
    Nodes.spec
  describe "Toolkit"
    Toolkit.spec
  describe "Toolkit2"
    Toolkit2.spec
  describe "Flex"
    Flex.spec
