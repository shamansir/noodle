module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Rpd.Test.Spec.Actions (spec) as TestActions
import Rpd.Test.Spec.Structure (spec) as TestStructure
import Rpd.Test.Spec.Flow (spec) as TestFlow
import Rpd.Test.Spec.Render (spec) as TestRender
import Rpd.Test.Spec.RpdFileParser (spec) as TestRpdFileParser
import Rpd.Test.Spec.Util (spec) as TestUtils
import Rpd.Test.Spec.Spread (spec) as TestSpreads
import Rpd.Test.Spec.FSM (spec) as TestFSM


spec :: Spec Unit
spec =
  describe "RPD" do
    TestActions.spec
    TestStructure.spec
    TestFlow.spec
    TestRender.spec
    TestRpdFileParser.spec
    TestUtils.spec
    TestSpreads.spec
    TestFSM.spec


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

