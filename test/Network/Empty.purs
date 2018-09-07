module RpdTest.Network.Empty
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it)

import Rpd (init, run') as R
import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R


data MyData
  = Bang

type MyRpd = R.Rpd (R.Network MyData)

myRpd :: MyRpd
myRpd =
  R.init "f"

spec :: Spec Unit
spec =
  describe "empty network" do
    it "constructing the network works" do
      -- FIXME: fail on error
      _ <- liftEffect $ R.run' (log <<< show) (const $ pure unit) myRpd
      pure unit
