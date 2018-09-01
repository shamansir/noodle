module RpdTest.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import RpdTest.Network.Empty (spec) as TestEmpty
import RpdTest.Network.Flow (spec) as TestFlow
import RpdTest.Network.Render (spec) as TestRender

main :: Effect Unit
main = run [consoleReporter] do
  describe "RPD" do
    TestEmpty.spec
    TestFlow.spec
    TestRender.spec
