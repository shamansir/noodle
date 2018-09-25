module RpdTest.Main where

import Prelude

import Test.Spec (Spec, describe)

import RpdTest.Network.Empty (spec) as TestEmpty
import RpdTest.Network.Flow (spec) as TestFlow
import RpdTest.Network.Render (spec) as TestRender

spec :: Spec Unit
spec =
  describe "RPD" do
    TestEmpty.spec
    TestFlow.spec
    TestRender.spec