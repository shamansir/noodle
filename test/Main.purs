module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Noodle.Test.Spec.Actions (spec) as TestActions
import Noodle.Test.Spec.Structure (spec) as TestStructure
import Noodle.Test.Spec.Flow (spec) as TestFlow
import Noodle.Test.Spec.Render (spec) as TestRender
import Noodle.Test.Spec.NoodleFileParser (spec) as TestNoodleFileParser
import Noodle.Test.Spec.Util (spec) as TestUtils
import Noodle.Test.Spec.Spread (spec) as TestSpreads
import Noodle.Test.Spec.FSM (spec) as TestFSM


spec :: Spec Unit
spec =
  describe "Noodle" do
    TestActions.spec
    TestStructure.spec
    TestFlow.spec
    TestRender.spec
    TestNoodleFileParser.spec
    TestUtils.spec
    TestSpreads.spec
    TestFSM.spec


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

