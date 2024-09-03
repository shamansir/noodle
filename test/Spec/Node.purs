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

import Test.MyToolkit.Node.Sample as Sample
import Test.MyToolkit.Node.Sum as Sum


spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            let
                rawShape =
                    Shape.reflect (Shape :: Sample.Shape)
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
            _ <- liftEffect $ Sample.makeNode $ pure unit
            pure unit

    describe "running" $ do

        it "running node with empty process function" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode $ pure unit
                Node.run myNode

        it "running node with some function (run with default values)" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode Sample.combineAll
                myNode # Node.run
                foo <- myNode # Node.atOutlet Sample.foo_out
                bar <- myNode # Node.atOutlet Sample.bar_out
                foo `shouldEqual` "35"
                bar `shouldEqual` -1

        it "running node with some function (send new values to all inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode Sample.combineAll
                myNode # Node.sendIn Sample.foo_in 7
                myNode # Node.sendIn Sample.bar_in "bar"
                myNode # Node.sendIn Sample.c_in 15
                myNode # Node.run
                foo <- myNode # Node.atOutlet Sample.foo_out
                bar <- myNode # Node.atOutlet Sample.bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (send new values to some inlets before running)" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode Sample.combineAll
                myNode # Node.listenUpdatesAndRun
                myNode # Node.sendIn Sample.foo_in 7
                myNode # Node.sendIn Sample.bar_in "bar"
                myNode # Node.run
                foo <- myNode # Node.atOutlet Sample.foo_out
                bar <- myNode # Node.atOutlet Sample.bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

        it "running node with some function (listen to updates and send values after that)" $ do
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode Sample.combineAll
                myNode # Node.listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode # Node.sendIn Sample.foo_in 7
                myNode # Node.sendIn Sample.bar_in "bar"
                myNode # Node.sendIn Sample.c_in 15
                foo <- myNode # Node.atOutlet Sample.foo_out
                bar <- myNode # Node.atOutlet Sample.bar_out
                foo `shouldEqual` "22bar"
                bar `shouldEqual` -8

        it "running node with some function (listen to updates and send some of the values)" $
            liftEffect $ do
                myNode <- liftEffect $ Sample.makeNode Sample.combineAll
                myNode # Node.listenUpdatesAndRun
                --Signal.runSignal $ (myNode # Node.logUpdates) ~> Console.log
                myNode # Node.sendIn Sample.foo_in 7
                myNode # Node.sendIn Sample.bar_in "bar"
                foo <- myNode # Node.atOutlet Sample.foo_out
                bar <- myNode # Node.atOutlet Sample.bar_out
                foo `shouldEqual` "9bar"
                bar `shouldEqual` 5

    describe "connecting & disconnecting" $ do


        it "is possible to connect nodes (case a)" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            _ <- Node.connect
                    Sum.sum_out
                    Sum.b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB # Node.atOutlet Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- so it's `a (2) + b (3)` from `nodeA` and `a (2) + b (2 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 2 + 3)

            pure unit

        it "is possible to connect nodes (case b)" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            nodeA # Node.sendIn Sum.a_in 4

            _ <- Node.connect
                    Sum.sum_out
                    Sum.b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB # Node.atOutlet Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `4` to the `a` inlet of `nodeA`
            -- so it's `a (4) + b (3)` from `nodeA` and `a (2) + b (4 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 4 + 3)

            pure unit

        it "is possible to connect nodes and keep sending values" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            nodeA # Node.sendIn Sum.a_in 4

            _ <- Node.connect
                    Sum.sum_out
                    Sum.b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB # Node.atOutlet sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            nodeA # Node.sendIn Sum.a_in 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB # Node.atOutlet Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `7` to the `a` inlet of `nodeA`
            -- (..to replace the value of `4` which was sent before)
            -- so it's `a (7) + b (3)` from `nodeA` and `a (2) + b (7 + 3)` from `nodeB`
            atSumB' `shouldEqual` (2 + 7 + 3)

            pure unit


        it "disconnecting works" $ liftEffect $ do
            (nodeA :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            (nodeB :: Sum.Node) <-
                Node.make Sum._sum unit (Shape :: Sum.Shape) { a : 2, b : 3 } { sum : 0 }
                    $ Sum.sumBoth

            -- Node.with nodeA $ P.sendIn _aI 4
            nodeA # Node.sendIn Sum.a_in 4

            link <- Node.connect
                    Sum.sum_out
                    Sum.b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB # Node.atOutlet sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            nodeA # Node.sendIn Sum.a_in 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB # Node.atOutlet Sum.sum_out
            -- it should act like the value that was sent after disconnecting was never received
            atSumB' `shouldEqual` (2 + 4 + 3)

            pure unit