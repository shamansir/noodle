module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.AVar (AVAR)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff ( console :: CONSOLE
                        , testOutput :: TESTOUTPUT
                        , avar :: AVAR
                        | eff) Unit
main = runTest do
  suite "sync code" do
    test "arithmetic" do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
      Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      Assert.equal (2 + 2) 4
      Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal (2 + 2) 5
  -- suite "async code" do
  --   test "with async IO" do
  --     fileContents <- FS.readTextFile UTF8 "file.txt"
  --     Assert.equal fileContents "hello here are your file contents"
  --   test "async operation with a timeout" do
  --     timeout 100 $ do
  --       file2Contents <- FS.readTextFile UTF8 "file2.txt"
  --       Assert.equal file2Contents "can we read a file in 100ms?"
