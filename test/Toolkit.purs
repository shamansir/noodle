module Test.Toolkit where

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
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Channel as Ch
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    describe "foo" $

        it "spawning works" $ do
            checkRef :: Ref Int <- liftEffect $ Ref.new 1
            let
                intChan = Ch.hot "int" 0
                toolkit :: Toolkit Unit Int
                toolkit =
                    Toolkit.register' (Toolkit.empty 0)
                        $ Fn.make' "sum"
                            -- TODO: withInlets / withInputs ...
                                -- -< "a" /\ intChan
                            [ Fn.in_ "a" /\ intChan
                            , Fn.in_ "b" /\ intChan
                            ]
                            -- TODO: withOutlets / withInputs ...
                                -- >- "a" /\ intChan
                            [ Fn.out_ "sum" /\ intChan
                            ]
                        $ do
                            a <- Fn.receive $ Fn.in_ "a"
                            b <- Fn.receive $ Fn.in_ "b"
                            Fn.send (Fn.out_ "sum") $ a + b
            maybeNode <- toolkit # Toolkit.spawn "sum" # liftEffect
            case maybeNode of
                Just node -> liftEffect $ do
                    Node.run node unit
                    Node.send node (Fn.in_ "a" /\ 5)
                    Node.send node (Fn.in_ "b" /\ 3)
                    sum <- Node.getO node (Fn.out_ "sum")
                    Console.log $ "ss" <> show sum
                    pure unit
                Nothing ->
                    pure unit

            pure unit

    describe "bar" $ do
        pure unit
