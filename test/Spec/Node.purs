module Test.Spec.Node where

import Prelude


import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
-- import Data.SOrder (type (:::), T)
import Data.Repr (class ToRepr, class FromRepr, class HasFallback, wrap, unwrap)
import Data.Repr (wrap, unwrap) as Repr

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

import Test.Spec (Spec, pending, describe, it, pending')
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
import Noodle.Node (make, run, Process, atInlet, atOutlet) as Node

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


import Test.Spec.Util.IntOrStringRepr (ISRepr(..))


type TestInlets = (I "foo" Hot Int ⟘ I "c" Hot Int ⟘  I "bar" Cold String ⟘ IS) :: Inlets
type TestOutlets = (O "foo" String ⟙ O "bar" Int ⟙ OS) :: Outlets


type TestInletsRow = ( foo :: Int, c :: Int, bar :: String )
type TestOutletsRow = ( foo :: String, bar :: Int )


type MyNode = Node "sum" Unit TestInletsRow TestOutletsRow ISRepr Effect
type MyNodeShape = Shape TestInlets TestOutlets


type MyProcess = Node.Process Unit TestInletsRow TestOutletsRow ISRepr Effect


spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            let
                rawShape =
                    Shape.reflect (Shape :: MyNodeShape)
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
            _ <- liftEffect $ makeMyNode $ pure unit
            pure unit

    describe "running" $ do

        it "running node with empty process function" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeMyNode $ pure unit
                Node.run myNode

        it "running node with some function" $ do
            liftEffect $ do
                myNode <- liftEffect $ makeMyNode $ do
                    foo <- Fn.receive foo_in
                    bar <- Fn.receive bar_in
                    c <- Fn.receive c_in
                    Fn.send foo_out $ show (foo + c) <> bar
                    Fn.send bar_out $ foo - c
                Node.run myNode
                foo <- Node.atOutlet foo_out myNode
                bar <- Node.atOutlet bar_out myNode
                foo `shouldEqual` "35"
                bar `shouldEqual` -1



foo_in = Fn.Inlet :: _ "foo"
bar_in = Fn.Inlet :: _ "bar"
c_in   = Fn.Inlet :: _ "c"
foo_out = Fn.Outlet :: _ "foo"
bar_out = Fn.Outlet :: _ "bar"



makeMyNode :: MyProcess -> Effect MyNode
makeMyNode =
    Node.make
        (Family :: _ "sum")
        unit
        (Shape :: MyNodeShape)
        { foo : 1, bar : "5", c : 2 }
        { foo : "1", bar : 12 }