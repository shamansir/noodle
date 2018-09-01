module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Rpd (init, Rpd, run, Network) as R
import Rpd.Log as RL
import Test.Spec (Spec, describe, it)

data MyData
  = Bang
  | Value Int

type MyRpd = R.Rpd (R.Network MyData)

myRpd :: MyRpd
myRpd =
  R.init "f"

spec :: Spec Unit
spec =
  describe "rendering" do
    it "constructing the network works" do
      _ <- liftEffect $ RL.runRpdLogging' myRpd
      pure unit
