module Test.Fn where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.State (modify_)

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Fn (Fn, Fn')
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Transfer as Fn

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    describe "foo" $

        it "summing works" $ do
            called1 :: Ref Boolean <- liftEffect $ Ref.new false
            called2 :: Ref Boolean <- liftEffect $ Ref.new false
            let
                fn :: forall m. MonadEffect m => Fn String String m Int
                fn =
                    Fn.make "foo" [ "a", "b" ] [ "sum" ] $ do
                        a <- Fn.receive "a"
                        b <- Fn.receive "b"
                        Fn.send "sum" $ a + b
                assertAt markRef expName expVal name val =
                    if (name == expName) then do
                        shouldEqual val expVal
                        Ref.write true markRef
                    else
                        fail $ "got none at " <> show name
            Fn.run
                0
                (Fn.s $ assertAt called1 "sum" 0)
                (Fn.r [ "a" /\ 0, "b" /\ 0 ])
                fn
            called1B <- liftEffect $ Ref.read called1
            shouldEqual called1B true
            Fn.run
                0
                (Fn.s $ assertAt called2 "sum" 8)
                (Fn.r [ "a" /\ 3, "b" /\ 5 ])
                fn
            called2B <- liftEffect $ Ref.read called2
            shouldEqual called2B true

    describe "bar" $ do
        pure unit
