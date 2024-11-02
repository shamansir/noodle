module Test.Fn where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.SOrder (type (:::), T)

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
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Id (Input(..), Output(..)) as Fn

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


{-
shouldContain :: forall state is os. String -> d -> Protocol.Tracker state is os -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Ref.read tracker
    case Map.lookup id values of
        Just otherVal ->
            if val == otherVal then pure unit
            else throwError $ error $ show val <> " /= " <> show otherVal
        Nothing ->
            throwError $ error $ "\"" <> id <> "\" was not found in tracker"
-}


spec :: Spec Unit
spec = do

    describe "foo" $ do

        it "summing works" $ do
            (tracker /\ protocol) <- liftEffect $ Protocol.make unit { a : 5, b : 3 } { sum : 0 }
            let
                fn :: forall m. MonadEffect m => SumFn m
                fn =
                    Fn.make "foo" sumOrders $ do
                        a <- Fn.receive a_in
                        b <- Fn.receive b_in
                        Fn.send sum_out $ a + b
            (_ /\ _ /\ outputs) <- Fn.run protocol fn
            outputs.sum `shouldEqual` 8

        it "summing works with sendIn" $ do
            (tracker /\ protocol) <- liftEffect $ Protocol.make unit { a : 0, b : 0 } { sum : 0 }
            let
                fn :: forall m. MonadEffect m => SumFn m
                fn =
                    Fn.make "foo" sumOrders $ do
                        Fn.sendIn a_in 6
                        Fn.sendIn b_in 7
                        a <- Fn.receive a_in
                        b <- Fn.receive b_in
                        Fn.send sum_out $ a + b
            (_ /\ inputs /\ outputs) <- Fn.run protocol fn
            outputs.sum `shouldEqual` 13
            inputs.a `shouldEqual` 6
            inputs.b `shouldEqual` 7

    describe "bar" $ do
        pure unit


type SumFn m =
    Fn Unit ( a :: Int, b :: Int ) ( sum :: Int ) m


a_in = Fn.Input 0 :: _ "a"
b_in = Fn.Input 1 :: _ "b"
sum_out = Fn.Output 0 :: _ "sum"


sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }