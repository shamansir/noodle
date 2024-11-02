module Test.Nodes where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (delay)

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT



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



type SignalNode a = Node (Maybe (Signal a))


createBangNode :: Effect (Node (Maybe Unit))
createBangNode = do
    Node.make Nothing
        $ D.fromFn
        $ \inlets ->
            case ("bang" <+ inlets) of
                Just Nothing ->
                    D.passNothing
                Just (Just _) -> do
                    D.pass [ "bang" /\ Just unit ]
                Nothing -> D.passNothing


createDelayNode :: forall a. Effect (SignalNode a)
createDelayNode = do
    node <- Node.make Nothing
        $ D.fromFn
        $ \inlets -> do
            case ("in" <+ inlets) of
                Just (Just signal) -> do
                    D.pass [ "out" /\ Just (signal # SignalT.delay 300.0) ]
                Just Nothing -> D.passNothing
                Nothing -> D.passNothing
    pure node


createProducingNode :: Effect (Node (Maybe Unit) /\ Signal (Effect Unit))
createProducingNode = do
    channel <- Ch.channel Nothing
    node <- Node.make Nothing
        $ D.fromFnEffectful
        $ \inlets -> do
            case ("trigger" <+ inlets) of
                Just (Just _) -> do
                    --launchAff_ $ delay $ Milliseconds 1000.0
                    Ch.send channel $ Just unit
                Just Nothing -> pure unit
                Nothing -> pure unit
            pure $ D.passNothing
    let
        producer =
            Ch.subscribe channel
            ~> ((/\) "out")
            ~> Node.produce node
    pure $ node /\ producer


spec :: Spec Unit
spec = do

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

    it "disconnecting" $ do
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

    it "triggering a value from outside" do
        producingNode /\ producer
            :: Node (Maybe Unit) /\ Signal (Effect Unit)
            <- liftEffect $ createProducingNode
        bangNode
            :: Node (Maybe Unit)
            <- liftEffect $ createBangNode
        _ <- liftEffect
            $ (producingNode /\ "out") <~> (bangNode /\ "bang") -- connect outlet `out` from `timer` node to inlet `bang` of the `bang` node
        let bangOut = Node.outletsSignal bangNode
        liftEffect $ do
            producingNode +> ( "trigger" /\ Just unit )
        expectFn bangOut [ Node.consumer /\ Nothing ]
        liftEffect $ Signal.runSignal producer
        expectFn bangOut [ "bang" /\ Just unit ]

    it "delaying" do
        delayingNode
            :: SignalNode Int
            <- liftEffect $ createDelayNode
        myChannel <- liftEffect $ Ch.channel 0
        let
            mySignal = Ch.subscribe myChannel
            outSignal = Node.outletSignal delayingNode "out"
        --expectFn outSignal [ Nothing ]
        liftEffect $ do
            delayingNode +> ( "in" /\ Just mySignal )
        maybeOut <- liftEffect $ Signal.get outSignal
        case maybeOut of
            (Just delayedSignal) -> do
                v1 <- liftEffect $ Signal.get delayedSignal
                v1 `shouldEqual` 0
                liftEffect $ Ch.send myChannel 5
                v2 <- liftEffect $ Signal.get delayedSignal
                v2 `shouldEqual` 0
                delay $ Milliseconds 400.0
                v3 <- liftEffect $ Signal.get delayedSignal
                v3 `shouldEqual` 5
                pure unit
            _ ->
                fail "foo"
        --expect

    pending "receiving and running a signal"

    pending "shaped: hot inlets"

    pending "shaped: cold inlets"

    pending "shaped: adapting"