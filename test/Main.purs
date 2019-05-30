module Test.Main where

import Prelude

import Effect (Effect)

import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import RpdTest.Structure (spec) as TestStructure
import RpdTest.Flow (spec) as TestFlow
import RpdTest.Render (spec) as TestRender
import RpdTest.RpdFileParser (spec) as TestRpdFileParser
import RpdTest.Util (spec) as TestUtils


spec :: Spec Unit
spec =
  describe "RPD" do
    TestStructure.spec
    TestFlow.spec
    TestRender.spec
    TestRpdFileParser.spec
    TestUtils.spec


main :: Effect Unit
main = run [consoleReporter] spec

