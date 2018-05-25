module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Control.Monad.Aff (delay)
import Control.Monad.Aff (Aff, attempt, delay, makeAff, runAff, launchAff, throwError, try)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Class (liftEff)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import FRP (FRP)

import Rpd (run) as Rpd

import RPDTest.Network.TestCreate (network) as TestCreate

main :: Eff (RunnerEffects (frp :: FRP, ref :: REF)) Unit
main = run [consoleReporter] do
  describe "RPD" do
    describe "creating" do
      it "constructing the network works" do
        -- makeAff, writeRef etc., see Signal tests
        res <- liftEff $ do
          testRef <- newRef "a"
          Rpd.run (\_ -> writeRef testRef "b") TestCreate.network
          readRef testRef
        res `shouldEqual` "Alligator"
    describe "connecting nodes" do
      pure unit
      -- it "runs in NodeJS" $ pure unit
      -- it "runs in the browser" $ pure unit
      -- it "supports streaming reporters" $ pure unit
      -- it "supports async specs" do
      --   delay (Milliseconds 100.0)
      --   res <-  pure "Alligator"
      --   res `shouldEqual` "Alligator"
      -- it "is PureScript 0.10.x compatible" $ pure unit
