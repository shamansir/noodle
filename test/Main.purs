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


createSumNode :: Effect (Node Int)
createSumNode =
    Node.fromFn 0
      $ \inlets ->
          Node.send'
            [ "c" /\ ((+) <$> "a" <| inlets
                          <*> "b" <| inlets
                     )
            ]


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do

      it "creating" do
        node :: Node Int
          <- liftEffect $ createSumNode
        let out = Node.outlets node
        expectFn out [ "bang" /\ 0 ]
        liftEffect $ do
          node |> ( "a" /\ 3 )
          node |> ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]

      it "connecting" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        _ <- liftEffect
          $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        liftEffect $ do
            nodeA |> ( "a" /\ 3 )
            nodeA |> ( "b" /\ 3 )
            nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ]

      it "connecting 2" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        _ <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ]
        liftEffect $ do
          nodeA |> ( "a" /\ 3 )
          nodeA |> ( "b" /\ 3 )
          nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ] -- sums up values
        liftEffect $ do
          nodeA |> ( "a" /\ 5 )
          nodeA |> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 16 ] -- recalculates the value
        liftEffect
          $ nodeB |> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 29 ] -- sums up new values

      it "disconnecting" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        link <- liftEffect
          $ (nodeA /\ "c") <~> (nodeB /\ "a") -- connect outlet `c` from Node A to inlet `a` from Node B
        let outB = Node.outlets nodeB
        expectFn outB [ "bang" /\ 0 ] -- expect default value to be there
        liftEffect $ do
          nodeA |> ( "a" /\ 3 )
          nodeA |> ( "b" /\ 3 )
          nodeB |> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ] -- sums up values
        liftEffect $ do
          Node.disconnect link -- disconnect
          nodeA |> ( "a" /\ 5 )
          nodeA |> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 10 ] -- doesn't recalculate
        liftEffect
          $ nodeB |> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 23 ] -- sums up with 10 which was stored in its `a` before connection

      pending "todo"
