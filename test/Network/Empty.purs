module RpdTest.Network.Empty
    ( spec ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Util (runWith, TestAffE)

import Rpd (init, Rpd, run, Network) as R

data MyData
  = Bang

type MyRpd e = R.Rpd e (R.Network MyData e)

myRpd :: forall e. MyRpd e
myRpd =
  R.init "f"

spec :: forall e. Spec (TestAffE e) Unit
spec =
  describe "empty network" do
    it "constructing the network works" do
      -- FIXME: fail on error
      _ <- liftEff $ R.run (log <<< show) (const $ pure unit) myRpd
      pure unit
