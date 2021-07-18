module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Control.Alternative ((<|>))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_, delay)

import Test.Spec (pending, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Signal (expectFn, expect)

import Noodle.Node.Define as D
import Noodle.Node.Shape (noInlets, noOutlets) as Shape
import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node (make, consumer, outletsSignal, disconnect, send, produce) as Node

import Signal ((~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch
import Signal.Channel.Extra as Ch



createSumNode :: Effect (Node Int)
createSumNode =
    Node.make 0
      $ D.fromFn
      $ \inlets ->
          D.pass'
            [ "c" /\ ((+) <$> "a" <+ inlets
                          <*> "b" <+ inlets
                     )
            ]

createTimerNode :: Effect (Node (Maybe Unit))
createTimerNode = do
    channel <- Ch.channel unit
    node <- Node.make Nothing
      $ D.fromFnEffectful
      $ \inlets -> do
          case ("trigger" <+ inlets) of
            Just _ -> do
              launchAff_ $ delay $ Milliseconds 1000.0
              Ch.send channel unit
            Nothing -> pure unit
          pure $ D.passNothing
    Signal.runSignal $
      Ch.subscribe channel
        ~> Just
        ~> ((/\) "out")
        ~> Node.produce node
    pure node


createBangNode :: Effect (Node (Maybe Unit))
createBangNode = do
    Node.make Nothing
      $ D.fromFn
      $ const
      $ D.pass [ "bang" /\ Just unit ]


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Noodle" do
    describe "Node" do

      it "creating" do
        node :: Node Int
          <- liftEffect $ createSumNode
        let out = Node.outletsSignal node
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
        let outB = Node.outletsSignal nodeB
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
        let outB = Node.outletsSignal nodeB
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
        let outB = Node.outletsSignal nodeB
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

      it "triggering a signal value" do
        timerNode :: Node (Maybe Unit)
          <- liftEffect $ createTimerNode
        bangNode :: Node (Maybe Unit)
          <- liftEffect $ createBangNode
        _ <- liftEffect
          $ (timerNode /\ "out") <~> (bangNode /\ "bang") -- connect outlet `out` from `timer` node to inlet `bang` of the `bang` node
        let bangOut = Node.outletsSignal bangNode
        liftEffect $ do
          timerNode +> ( "trigger" /\ Just unit )
        expectFn bangOut [ "bang" /\ Nothing ]
        expect 1100 bangOut [ "bang" /\ Just unit ]

      pending "receiving and running a signal"

      pending "shaped: hot inlets"

      pending "shaped: cold inlets"

      pending "shaped: adapting"