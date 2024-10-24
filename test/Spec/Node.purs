module Test.Spec.Node where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Map (empty, insert) as Map
import Noodle.Repr (Repr(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Id as Id
import Noodle.Fn.Shape (Shape(..), InletR(..), OutletR(..))
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Raw.Fn.Shape (inlets, outlets, make) as RawShape
import Noodle.Raw.Fn.Process (receive, send, sendIn) as RawFn
import Noodle.Id (Temperament(..))
import Noodle.Node (Node, (<-#), (<-@), (#->), (@->), (<=#), (<=@), (<~>))
import Noodle.Node (connect, disconnect, _listenUpdatesAndRun, make, run, state, modifyState, atOutletR) as Node
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (run, make, state, atInlet, atOutlet) as RawNode

import Example.Toolkit.Minimal.Repr (MinimalRepr)
import Example.Toolkit.Minimal.Repr (MinimalRepr(..)) as MinimalRepr
import Example.Toolkit.Minimal.Node.Sample as Sample
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Stateful as Stateful


spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            let
                rawShape =
                    Shape.reflect (Shape :: Sample.Shape)
            RawShape.inlets rawShape `shouldEqual`
                [ { name : "foo", order : 0, temp : Hot }
                , { name : "c"  , order : 1, temp : Hot }
                , { name : "bar", order : 2, temp : Cold }
                ]
            RawShape.outlets rawShape `shouldEqual`
                [ { name : "foo", order : 0 }
                , { name : "bar", order : 1 }
                ]

    describe "creation" $ do

        it "creating node" $ do
            _ <- liftEffect $ Sample.makeNode' $ pure unit
            pure unit

    describe "running" $ do

        it "running node with empty process function" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode' $ pure unit
                Node.run myNode

        it "running node with some function (run with default values)" $ do
            liftEffect $ do
                myNode <- liftEffect Sample.makeNode
                myNode # Node.run
                foo <- myNode <-@ Sample.foo_out
                bar <- myNode <-@ Sample.bar_out
                -- foo <- myNode <=@ _.foo
                -- bar <- myNode <=@ _.bar
                foo `shouldEqual` "35"
                bar `shouldEqual` -1

        it "running node with some function (send new values to all inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect Sample.makeNode
                myNode #-> Sample.foo_in /\ 7
                myNode #-> Sample.bar_in /\ "bar"
                myNode #-> Sample.c_in   /\ 15
                myNode # Node.run
                foo <- myNode <-@ Sample.foo_out
                bar <- myNode <-@ Sample.bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (send new values to some inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect Sample.makeNode
                myNode # Node._listenUpdatesAndRun
                myNode #-> Sample.foo_in /\ 7
                myNode #-> Sample.bar_in /\ "bar"
                myNode # Node.run
                foo <- myNode <-@ Sample.foo_out
                bar <- myNode <-@ Sample.bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

        it "running node with some function (listen to updates and send values after that)" $ do
            liftEffect $ do
                myNode <- liftEffect Sample.makeNode
                myNode # Node._listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode #-> Sample.foo_in /\ 7
                myNode #-> Sample.bar_in /\ "bar"
                myNode #-> Sample.c_in   /\ 15
                foo <- myNode <-@ Sample.foo_out
                bar <- myNode <-@ Sample.bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (listen to updates and send some of the values)" $
            liftEffect $ do
                myNode <- liftEffect Sample.makeNode
                myNode # Node._listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode #-> Sample.foo_in /\ 7
                myNode #-> Sample.bar_in /\ "bar"
                foo <- myNode <-@ Sample.foo_out
                bar <- myNode <-@ Sample.bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

    describe "connecting & disconnecting" $ do

        it "is possible to connect nodes (case a)" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            _ <- (nodeA /\ Sum.sum_out)
                 <~>
                 (nodeB /\ Sum.b_in)

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- so it's `a (2) + b (3)` from `nodeA` and `a (2) + b (2 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 2 + 3)

            pure unit

        it "is possible to connect nodes (case b)" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            _ <- (nodeA /\ Sum.sum_out)
                 <~>
                 (nodeB /\ Sum.b_in)

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `4` to the `a` inlet of `nodeA`
            -- so it's `a (4) + b (3)` from `nodeA` and `a (2) + b (4 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 4 + 3)

            pure unit

        it "is possible to connect nodes and keep sending values" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            _ <- (nodeA /\ Sum.sum_out)
                 <~>
                 (nodeB /\ Sum.b_in)

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB <-@ sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            nodeA #-> Sum.a_in /\ 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `7` to the `a` inlet of `nodeA`
            -- (..to replace the value of `4` which was sent before)
            -- so it's `a (7) + b (3)` from `nodeA` and `a (2) + b (7 + 3)` from `nodeB`
            atSumB' `shouldEqual` (2 + 7 + 3)

            pure unit


        it "disconnecting works" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            link <- (nodeA /\ Sum.sum_out)
                    <~>
                    (nodeB /\ Sum.b_in)

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB <-@ sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            nodeA #-> Sum.a_in /\ 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB <-@ Sum.sum_out
            -- it should act like the value that was sent after disconnecting was never received
            atSumB' `shouldEqual` (2 + 4 + 3)

            pure unit

    describe "working with state" $ do

        it "modifying state in the node processing function" $ liftEffect $ do
            (statefulNode :: Stateful.Node) <- Stateful.makeNode
            stateA <- Node.state statefulNode
            stateA `shouldEqual` "x"
            statefulNode # Node._listenUpdatesAndRun
            stateB <- Node.state statefulNode
            stateB `shouldEqual` "x-0-0"
            _ <- statefulNode #-> Stateful.a_in /\ 5
            _ <- statefulNode #-> Stateful.b_in /\ 7
            stateC <- Node.state statefulNode
            stateC `shouldEqual` "x-0-0-5-12"

        it "modifying state from outside" $ liftEffect $ do
            (statefulNode :: Stateful.Node) <- Stateful.makeNode
            statefulNode # Node._listenUpdatesAndRun
            stateBefore <- Node.state statefulNode
            stateBefore `shouldEqual` "x-0-0"
            statefulNode # Node.modifyState ((<>) "***-")
            _ <- statefulNode #-> Stateful.a_in /\ 5
            _ <- statefulNode #-> Stateful.b_in /\ 7
            stateC <- Node.state statefulNode
            stateC `shouldEqual` "***-x-0-0-5-12"

    describe "raw nodes" $ do

        it "is possible to create raw node" $ liftEffect $ do
            (rawNode :: Raw.Node MinimalRepr Effect) <-
                RawNode.make (Id.FamilyR { family : "myRawNode" })
                    MinimalRepr.None
                    (RawShape.make { inlets : [], outlets : [] }) -- TODO
                    (Map.empty
                        # Map.insert (InletR "a") (MinimalRepr.Int 5)
                        # Map.insert (InletR "b") (MinimalRepr.Int 7)
                    )
                    (Map.empty
                        # Map.insert (OutletR "sum") (MinimalRepr.Int 0)
                    )
                    $ do
                        mbA <- RawFn.receive $ InletR "a"
                        mbB <- RawFn.receive $ InletR "b"
                        RawFn.send (OutletR "sum") $ Repr $ MinimalRepr.Int $ case mbA /\ mbB of
                            (Repr (MinimalRepr.Int a) /\ Repr (MinimalRepr.Int b)) -> a + b
                            _ -> 0

            rawNode # RawNode.run

            mbSum <- RawNode.atOutlet (OutletR "sum") rawNode

            mbSum `shouldEqual` (Just $ MinimalRepr.Int 12)