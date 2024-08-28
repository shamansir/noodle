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
import Noodle.Fn.Shape (Shape(..), type (⟘), type (⟙), IS, OS, I, O, Hot, Cold, Inlets, Outlets)
import Noodle.Fn.Shape (reflect, inlets, outlets) as Shape
import Noodle.Id (Inlet(..), Outlet(..)) as Fn
import Noodle.Id (Family(..), Temperament(..))
import Noodle.Node (Node)
import Noodle.Node
    ( make, run, Process
    , atInlet, atOutlet, sendIn, sendOut
    , listenUpdatesAndRun, runOnInletUpdates
    , connect, connectAlike
    , logUpdates
    ) as Node

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


import Test.Spec.Util.IntOrStringRepr (ISRepr(..))


type SumInlets  =
    ( I "a" Hot Int
    ⟘ I "b" Hot Int
    ⟘ IS
    ) :: Inlets
type SumOutlets =
    ( O "sum" Int
    ⟙ OS
    ) :: Outlets
type SumNodeShape = Shape SumInlets SumOutlets
type SumNode = Node "sum" Unit ( a :: Int, b :: Int ) ( sum :: Int ) Int Effect
type SumProcess = Node.Process Unit ( a :: Int, b :: Int ) ( sum :: Int ) Int Effect


type SampleInlets  =
    ( I "foo" Hot Int
    ⟘ I "c" Hot Int
    ⟘ I "bar" Cold String
    ⟘ IS
    ) :: Inlets
type SampleOutlets =
    ( O "foo" String
    ⟙ O "bar" Int
    ⟙ OS
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
type SampleNode = Node "sample" Unit SampleInletsRow SampleOutletsRow ISRepr Effect
type SampleProcess = Node.Process Unit SampleInletsRow SampleOutletsRow ISRepr Effect


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

        it "is possible to connect nodes" $ liftEffect $ do
            (nodeA :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- Fn.receive a_in
                        b <- Fn.receive b_in
                        Fn.send sum_out $ a + b

            (nodeB :: SumNode) <-
                Node.make _sum unit (Shape :: SumNodeShape) { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- Fn.receive a_in
                        b <- Fn.receive b_in
                        Fn.send sum_out $ a + b

            nodeA # Node.sendIn a_in 4

            _ <- Node.connect
                    sum_out
                    b_in
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: optional: now `connect` runs the second node automatically

            atSumB <- nodeB # Node.atOutlet sum_out
            atSumB `shouldEqual` (4 + 3 + 2)

            pure unit

    {-
        it "is possible to connect nodes and keep sending values" $ do
            nodeA <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            _ <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (7 + 3 + 2)

            pure unit

        it "disconnecting works" $ do
            nodeA <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            link <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (4 + 3 + 2)

            pure unit
        -}


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
