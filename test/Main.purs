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

import Node (fromFn, receive, send, send', outlets) as Node
import Node (Node, (|>), (<|), (<~>), (+>), (<+))


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do
      it "creating" do
        node :: Node Int
          <- liftEffect
              $ Node.fromFn 0
              $ \inlets ->
                pure $ Node.send'
                    [ "c" /\ ((+)
                                  <$> "a" <| inlets
                                  <*> "b" <| inlets
                             )
                    ]
        let out = Node.outlets node
        expectFn out [ "bang" /\ 0 ]
        _ <- liftEffect $ node |> ( "a" /\ 3 )
        _ <- liftEffect $ node |> ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]
      it "connecting" do
        nodeA :: Node Int
          <- liftEffect
              $ Node.fromFn 0
              $ \inlets ->
                pure $ Node.send'
                    [ "c" /\ ((+)
                                  <$> "a" <| inlets
                                  <*> "b" <| inlets
                             )
                    ]
        nodeB :: Node Int
          <- liftEffect
              $ Node.fromFn 0
              $ \inlets ->
                pure $ Node.send'
                    [ "c" /\ ((+)
                                  <$> "a" <| inlets
                                  <*> "b" <| inlets
                             )
                    ]
                -- pure $ const $ (+) <$> receive "a" <*> receive "b"
        _ <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        _ <- liftEffect $ nodeA |> ( "a" /\ 3 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 3 )
        _ <- liftEffect $ nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 7 ]
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
