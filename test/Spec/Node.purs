module Test.Spec.Node where

import Prelude


import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
-- import Data.SOrder (type (:::), T)
import Data.Repr (class ToRepr, class FromRepr, class HasFallback, wrap, unwrap)
import Data.Repr (wrap, unwrap) as Repr
import Data.Traversable (traverse)

import Type.Proxy (Proxy(..))

import Control.Monad.State (modify_)
import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (Error)
import Effect.Aff (Aff, throwError, error)

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
-- import Noodle.Node (Node)
-- import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Fn.Shape (Shape(..), type (/.), type (\.), IS, OS, I, O, Hot, Cold, Inlets, Outlets)
import Noodle.Fn.Shape (reflect, inlets, outlets) as Shape
import Noodle.Id (Inlet(..), Outlet(..)) as Fn
import Noodle.Id (Family(..), Temperament(..))
import Noodle.Node (Node)
import Noodle.Node
    ( make, run
    , atInlet, atOutlet, sendIn, sendOut
    , listenUpdatesAndRun, runOnInletUpdates
    , connect, connectAlike, disconnect
    , logUpdates
    ) as Node

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


import Test.Spec.Util.IntOrStringRepr (ISRepr(..))


type SumInlets  =
    (  I "a" Hot Int
    /. I "b" Hot Int
    /. IS
    ) :: Inlets
type SumOutlets =
    (  O "sum" Int
    \. OS
    ) :: Outlets
type SumNodeShape = Shape SumInlets SumOutlets
type SumNode    = Node "sum"   Unit ( a :: Int, b :: Int ) ( sum :: Int ) Int Effect
type SumProcess = Fn.Process   Unit ( a :: Int, b :: Int ) ( sum :: Int ) Int Effect


type SampleInlets  =
    (  I "foo" Hot Int
    /. I "c" Hot Int
    /. I "bar" Cold String
    /. IS
    ) :: Inlets
type SampleOutlets =
    (  O "foo" String
    \. O "bar" Int
    \. OS
    ) :: Outlets
type SampleInletsRow =
    ( foo :: Int
    , c :: Int
    , bar :: String
    )
type SampleOutletsRow =
    ( foo :: String
    , bar :: Int
    )
type SampleNodeShape = Shape SampleInlets SampleOutlets
type SampleNode    = Node "sample" Unit SampleInletsRow SampleOutletsRow ISRepr Effect
type SampleProcess = Fn.Process    Unit SampleInletsRow SampleOutletsRow ISRepr Effect


spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            let
                rawShape =
                    Shape.reflect (Shape :: SampleNodeShape)
            Shape.inlets rawShape `shouldEqual`
                [ { name : "foo", order : 0, temp : Hot }
                , { name : "c"  , order : 1, temp : Hot }
                , { name : "bar", order : 2, temp : Cold }
                ]
            Shape.outlets rawShape `shouldEqual`
                [ { name : "foo", order : 0 }
                , { name : "bar", order : 1 }
                ]

    describe "creation" $ do

        it "creating node" $ do
            _ <- liftEffect $ makeSampleNode $ pure unit
            pure unit

    describe "running" $ do

        it "running node with empty process function" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode $ pure unit
                Node.run myNode

        let
            combineAll :: SampleProcess
            combineAll = do
                foo <- Fn.receive foo_in
                bar <- Fn.receive bar_in
                c <- Fn.receive c_in
                -- liftEffect $ Console.log $ "run : <foo> " <> show foo <> " <bar> " <> show bar <> " <c> " <> show c
                Fn.send foo_out $ show (foo + c) <> bar
                Fn.send bar_out $ foo - c

        it "running node with some function (run with default values)" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode combineAll
                myNode # Node.run
                foo <- myNode # Node.atOutlet foo_out
                bar <- myNode # Node.atOutlet bar_out
                foo `shouldEqual` "35"
                bar `shouldEqual` -1

        it "running node with some function (send new values to all inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode combineAll
                myNode # Node.sendIn foo_in 7
                myNode # Node.sendIn bar_in "bar"
                myNode # Node.sendIn c_in 15
                myNode # Node.run
                foo <- myNode # Node.atOutlet foo_out
                bar <- myNode # Node.atOutlet bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (send new values to some inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode combineAll
                myNode # Node.listenUpdatesAndRun
                myNode # Node.sendIn foo_in 7
                myNode # Node.sendIn bar_in "bar"
                myNode # Node.run
                foo <- myNode # Node.atOutlet foo_out
                bar <- myNode # Node.atOutlet bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

        it "running node with some function (listen to updates and send values after that)" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode combineAll
                myNode # Node.listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode # Node.sendIn foo_in 7
                myNode # Node.sendIn bar_in "bar"
                myNode # Node.sendIn c_in 15
                foo <- myNode # Node.atOutlet foo_out
                bar <- myNode # Node.atOutlet bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (listen to updates and send some of the values)" $
            liftEffect $ do
                myNode <- liftEffect $ makeSampleNode combineAll
                myNode # Node.listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode # Node.sendIn foo_in 7
                myNode # Node.sendIn bar_in "bar"
                foo <- myNode # Node.atOutlet foo_out
                bar <- myNode # Node.atOutlet bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

    describe "connecting & disconnecting" $ do

        let
            sumBoth :: SumProcess
            sumBoth = do
                a <- Fn.receive a_in
                b <- Fn.receive b_in
                Fn.send sum_out $ a + b


        it "is possible to connect nodes (case a)" $ liftEffect $ do
            (nodeA :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            (nodeB :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            _ <- Node.connect
                    sum_out
                    b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB # Node.atOutlet sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- so it's `a (2) + b (3)` from `nodeA` and `a (2) + b (2 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 2 + 3)

            pure unit

        it "is possible to connect nodes (case b)" $ liftEffect $ do
            (nodeA :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            (nodeB :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            nodeA # Node.sendIn a_in 4

            _ <- Node.connect
                    sum_out
                    b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB # Node.atOutlet sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `4` to the `a` inlet of `nodeA`
            -- so it's `a (4) + b (3)` from `nodeA` and `a (2) + b (4 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 4 + 3)

            pure unit

        it "is possible to connect nodes and keep sending values" $ liftEffect $ do
            (nodeA :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            (nodeB :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            nodeA # Node.sendIn a_in 4

            _ <- Node.connect
                    sum_out
                    b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB # Node.atOutlet sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            nodeA # Node.sendIn a_in 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB # Node.atOutlet sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `7` to the `a` inlet of `nodeA`
            -- (..to replace the value of `4` which was sent before)
            -- so it's `a (7) + b (3)` from `nodeA` and `a (2) + b (7 + 3)` from `nodeB`
            atSumB' `shouldEqual` (2 + 7 + 3)

            pure unit


        it "disconnecting works" $ liftEffect $ do
            (nodeA :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            (nodeB :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ sumBoth

            -- Node.with nodeA $ P.sendIn _aI 4
            nodeA # Node.sendIn a_in 4

            link <- Node.connect
                    sum_out
                    b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB # Node.atOutlet sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            nodeA # Node.sendIn a_in 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB # Node.atOutlet sum_out
            -- it should act like the value that was sent after disconnecting was never received
            atSumB' `shouldEqual` (2 + 4 + 3)

            pure unit


foo_in  = Fn.Inlet :: _ "foo"
bar_in  = Fn.Inlet :: _ "bar"
c_in    = Fn.Inlet :: _ "c"
a_in    = Fn.Inlet :: _ "a"
b_in    = Fn.Inlet :: _ "b"

foo_out = Fn.Outlet :: _ "foo"
bar_out = Fn.Outlet :: _ "bar"
sum_out = Fn.Outlet :: _ "sum"

_sample = Family :: _ "sample"
_sum    = Family :: _ "sum"


makeSampleNode :: SampleProcess -> Effect SampleNode
makeSampleNode =
    Node.make
        _sample
        unit
        (Shape :: SampleNodeShape)
        { foo : 1, bar : "5", c : 2 }
        { foo : "1", bar : 12 }


makeSumNode :: SumProcess -> Effect SumNode
makeSumNode =
    Node.make
        _sum
        unit
        (Shape :: SumNodeShape)
        { a : 0, b : 0 }
        { sum : 0 }
