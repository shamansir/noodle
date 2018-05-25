module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, attempt, delay, makeAff, runAff, launchAff, throwError, try)
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import FRP (FRP)
import Rpd (run, empty, Network) as Rpd
import Rpd.Flow (Subscribers, subscribeAll) as Rpd
import RpdTest.Network.Empty (network) as TestEmpty
import RpdTest.Network.Flow (network) as TestFlow
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)


type TestAffE e = (ref :: REF, frp :: FRP | e)
type TestAff e = Aff (TestAffE e) Unit


main :: forall e. Eff (RunnerEffects (TestAffE e)) Unit
main = run [consoleReporter] do
  describe "RPD" do
    describe "creating" do
      it "constructing the network works" do
        runWith TestEmpty.network
          \nw -> pure unit
    describe "subscribing to the data flow" do
      it "receives the data from events" do
        -- TODO: move tests for a network in the module with this network, export as a suite
        runWith TestFlow.network
          \nw ->
            do
              collectedData <- liftEff $ do
                collectedData <- newRef []
                let
                  onInletData path source d = do pure unit
                  onOutletData path d = do pure unit
                  subscribers = Rpd.subscribeAll onInletData onOutletData nw
                _ <- performSubs subscribers
                readRef collectedData
              -- collectedData `shouldEqual` []
              pure unit
    describe "connecting channels after creation" do
      pure unit
    describe "disconnecting channels after creation" do
      pure unit
    describe "manually sending data to the channels after creation" do
      pure unit
    describe "manually sending delayed data to the channels after creation" do
      --   delay (Milliseconds 100.0)
      pure unit
    describe "adding nodes after creation" do
      pure unit
    describe "deleting nodes after creation" do
      pure unit
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


runWith :: forall e d. Rpd.Network d -> (Rpd.Network d -> TestAff e) -> TestAff e
runWith initialNetwork f = do
  newNetwork <- liftEff $ do
    networkRef <- newRef Rpd.empty
    Rpd.run (writeRef networkRef) initialNetwork
    readRef networkRef
  f newNetwork


performSubs :: forall e. Rpd.Subscribers e -> Eff e Unit
performSubs subs = do
  pure unit
