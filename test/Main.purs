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

import Node (fromFn, receive, send, send', outlets, disconnect) as Node
import Node (Node, (|>), (<|), (<~>), (+>), (<+))


createNode :: Effect (Node Int)
createNode =
    Node.fromFn 0
      $ \inlets ->
          pure $ Node.send'
            [ "c" /\ ((+)
                          <$> "a" <| inlets
                          <*> "b" <| inlets
                      )
            ]


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do
      it "creating" do
        node :: Node Int
          <- liftEffect $ createNode
        let out = Node.outlets node
        expectFn out [ "bang" /\ 0 ]
        _ <- liftEffect $ node |> ( "a" /\ 3 )
        _ <- liftEffect $ node |> ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]
      it "connecting" do
        nodeA :: Node Int
          <- liftEffect $ createNode
        nodeB :: Node Int
          <- liftEffect $ createNode
        _ <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        _ <- liftEffect $ nodeA |> ( "a" /\ 3 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 3 )
        _ <- liftEffect $ nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ]
      it "connecting 2" do
        nodeA :: Node Int
          <- liftEffect $ createNode
        nodeB :: Node Int
          <- liftEffect $ createNode
        link <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        _ <- liftEffect $ nodeA |> ( "a" /\ 3 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 3 )
        _ <- liftEffect $ nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ]
        _ <- liftEffect $ nodeA |> ( "a" /\ 5 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 16 ] -- recalculates the value
        _ <- liftEffect $ nodeB |> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 29 ] -- sums up new values
        _ <- liftEffect $ Node.disconnect link
        pure unit
      it "disconnecting" do
        nodeA :: Node Int
          <- liftEffect $ createNode
        nodeB :: Node Int
          <- liftEffect $ createNode
        link <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        _ <- liftEffect $ nodeA |> ( "a" /\ 3 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 3 )
        _ <- liftEffect $ nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ]
        _ <- liftEffect $ Node.disconnect link
        _ <- liftEffect $ nodeA |> ( "a" /\ 5 )
        _ <- liftEffect $ nodeA |> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 10 ] -- doesn't recalculate
        _ <- liftEffect $ nodeB |> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 23 ] -- sums up with 10 which was stored in its `a`
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
