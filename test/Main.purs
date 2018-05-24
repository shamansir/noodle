module Test.Main where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

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
        delay (Milliseconds 100.0)
        res <-  pure "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.10.x compatible" $ pure unit
