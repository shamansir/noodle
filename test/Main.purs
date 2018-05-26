module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Test.Util (TestAffE)

import RpdTest.Network.Empty (spec) as TestEmpty
import RpdTest.Network.Flow (spec) as TestFlow

main :: forall e. Eff (RunnerEffects (TestAffE e)) Unit
main = run [consoleReporter] do
  describe "RPD" do
    TestEmpty.spec
    TestFlow.spec
    describe "processing the output from nodes" do
      describe "with predefined function" do
        pure unit
      describe "with function defined after creation" do
        pure unit
      describe "after adding an outlet" do
        pure unit
      describe "after removing an outlet" do
        pure unit
      describe "after changing the node structure" do
        pure unit
      describe "after deleting the receiving node" do
        pure unit
      describe "after adding new node" do
        pure unit
