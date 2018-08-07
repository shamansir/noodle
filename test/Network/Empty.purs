module RpdTest.Network.Empty
    ( spec ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Util (runWith, TestAffE)

import Rpd (Network, init) as R

data MyData
  = Bang

network :: forall e. R.Network MyData e
network =
  R.init "f"

spec :: forall e. Spec (TestAffE e) Unit
spec =
  describe "empty network" do
    it "constructing the network works" do
      runWith network
        \nw -> pure unit
