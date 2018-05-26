module RpdTest.Network.Empty
    ( spec ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Util (runWith, TestAffE)

import Rpd (empty, Network) as R

data MyData
  = Bang

network :: R.Network MyData
network =
  R.empty

spec :: forall e. Spec (TestAffE e) Unit
spec =
  describe "empty network" do
    it "constructing the network works" do
      runWith network
        \nw -> pure unit
