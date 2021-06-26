module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Identity (Identity)
import Data.Tuple.Nested ((/\))
import Signal.Channel as Channel

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_, delay)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Signal

import Node (fromFn, receive, send) as Node
import Node (Node, NodeDef)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do
      it "creating" do
        def :: NodeDef Int
          <- liftEffect
              $ Node.fromFn 0
              $ \i ->
                pure $ Node.send
                    [ "c" /\ fromMaybe 0 ((+) <$> Node.receive "a" i <*> Node.receive "b" i) ]
                -- pure $ const $ (+) <$> receive "a" <*> receive "b"
        let out = Channel.subscribe def.out
        expectFn out [ "c" /\ 0 ]
        _ <- liftEffect $ Channel.send def.in ( "a" /\ 3 )
        _ <- liftEffect $ Channel.send def.in ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]
      it "connecting" do
        defA :: NodeDef Int
          <- liftEffect
              $ Node.fromFn 0
              $ \i ->
                pure $ Node.send
                    [ "c" /\ fromMaybe 0 ((+) <$> Node.receive "a" i <*> Node.receive "b" i) ]
        defB :: NodeDef Int
          <- liftEffect
              $ Node.fromFn 0
              $ \i ->
                pure $ Node.send
                    [ "c" /\ fromMaybe 0 ((+) <$> Node.receive "a" i <*> Node.receive "b" i) ]
                -- pure $ const $ (+) <$> receive "a" <*> receive "b"
        _ <- liftEffect $ Node.connect (defA.node /\ "c") (defB.node /\ "a")
        let out = Channel.subscribe defB.out
        expectFn out [ "c" /\ 0 ]
        _ <- liftEffect $ Channel.send defA.in ( "a" /\ 3 )
        _ <- liftEffect $ Channel.send defA.in ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]
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
