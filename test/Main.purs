module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))
import Data.Identity (Identity)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_, delay)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node (fromFn) as Node
import Node (Node)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do
      it "creating" do
        node :: Node Identity Int
          <- liftEffect
              $ Node.fromFn 0
              $ \receive ->
                let
                  send "out" = (+) <$> receive "a" <*> receive "b"
                  send _ = Nothing
                in pure send
                -- pure $ const $ (+) <$> receive "a" <*> receive "b"
        pure unit
      pending "todo"
    {- describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        res <- delay (Milliseconds 100.0) $> "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.12.x compatible" $ pure unit
    -}
