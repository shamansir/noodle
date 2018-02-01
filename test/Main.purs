module Test.Main where

import Prelude

import Data.Time.Duration as Duration

import Control.Monad.Aff (Aff, delay, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Signal as S
import Signal.Channel as SC
import Signal.Time (debounce)

import Test.Signal (expect')

--import Rpd

main :: forall eff. Eff (RunnerEffects ( ref :: REF, channel :: SC.CHANNEL | eff )) Unit
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
      it "tests signals"
        $ expect' (S.constant "lol") ["lol"]
      it "tests async signals" $ do
        chan <- liftEff $ SC.channel 0
        let sig = debounce 10.0 $ SC.subscribe chan
            send' = liftEff <<< SC.send chan

        _ <- forkAff $ expect' sig [0,2,4]
        wait 20.0
        send' 1
        wait 5.0
        send' 2
        wait 20.0
        send' 3
        wait 5.0
        send' 4
        wait 20.0


wait :: forall e. Number -> Aff e Unit
wait t = do
  delay (Duration.Milliseconds t)
  pure unit
