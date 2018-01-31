module Test.Main where

import Prelude

import Data.Time.Duration as Duration

import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.AVar (AVAR)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

--import Rpd

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        delay (Duration.Milliseconds 100.0)
        res <- pure "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.10.x compatible" $ pure unit

-- main :: forall eff. Eff ( console :: CONSOLE
--                         , testOutput :: TESTOUTPUT
--                         , avar :: AVAR
--                         | eff) Unit
-- main = runTest do
--   suite "API" do
--     test "initialization" do
--       Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
--       Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
--       Assert.equal (2 + 2) 4
--       Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal (2 + 2) 5
  -- suite "async code" do
  --   test "with async IO" do
  --     fileContents <- FS.readTextFile UTF8 "file.txt"
  --     Assert.equal fileContents "hello here are your file contents"
  --   test "async operation with a timeout" do
  --     timeout 100 $ do
  --       file2Contents <- FS.readTextFile UTF8 "file2.txt"
  --       Assert.equal file2Contents "can we read a file in 100ms?"
