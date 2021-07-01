module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)

import Test.Spec (pending, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Signal (expectFn)

import Noodle.Node (pass', outlets, disconnect, consumer) as Node
import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node.Unit (Node)
import Noodle.Node.Unit (make) as Node


createSumNode :: Effect (Node Int)
createSumNode =
    Node.make 0
      $ \inlets ->
          Node.pass'
            [ "c" /\ ((+) <$> "a" <+ inlets
                          <*> "b" <+ inlets
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
        expectFn out [ Node.consumer /\ 0 ]
        liftEffect $ do
          node +> ( "a" /\ 3 )
          node +> ( "b" /\ 4 )
        expectFn out [ "c" /\ 7 ]

      it "connecting" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        _ <- liftEffect
          $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ Node.consumer /\ 0 ]
        liftEffect $ do
            nodeA +> ( "a" /\ 3 )
            nodeA +> ( "b" /\ 3 )
            nodeB +> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ]

      it "connecting 2" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        _ <- liftEffect $ (nodeA /\ "c") <~> (nodeB /\ "a")
        let outB = Node.outlets nodeB
        expectFn outB [ Node.consumer /\ 0 ]
        liftEffect $ do
          nodeA +> ( "a" /\ 3 )
          nodeA +> ( "b" /\ 3 )
          nodeB +> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ] -- sums up values
        liftEffect $ do
          nodeA +> ( "a" /\ 5 )
          nodeA +> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 16 ] -- recalculates the value
        liftEffect
          $ nodeB +> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 29 ] -- sums up new values

      it "disconnecting" do
        nodeA :: Node Int
          <- liftEffect $ createSumNode
        nodeB :: Node Int
          <- liftEffect $ createSumNode
        link <- liftEffect
          $ (nodeA /\ "c") <~> (nodeB /\ "a") -- connect outlet `c` from Node A to inlet `a` from Node B
        let outB = Node.outlets nodeB
        expectFn outB [ Node.consumer /\ 0 ] -- expect default value to be there
        liftEffect $ do
          nodeA +> ( "a" /\ 3 )
          nodeA +> ( "b" /\ 3 )
          nodeB +> ( "b" /\ 4 )
        expectFn outB [ "c" /\ 10 ] -- sums up values
        liftEffect $ do
          Node.disconnect link -- disconnect
          nodeA +> ( "a" /\ 5 )
          nodeA +> ( "b" /\ 7 )
        expectFn outB [ "c" /\ 10 ] -- doesn't recalculate
        liftEffect
          $ nodeB +> ( "b" /\ 17 )
        expectFn outB [ "c" /\ 23 ] -- sums up with 10 which was stored in its `a` before connection

      pending "receiving and running a signal"
