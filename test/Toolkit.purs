module Test.Toolkit where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.State (modify_, get) as State

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

    describe "foo" $ do

        it "spawning works" $ do
            let
                intChan = Ch.hot "int" 0
                toolkit :: Toolkit Unit Int
                toolkit =
                    Toolkit.registerFn (Toolkit.empty "Ints" unit 0) "---"
                        $ Fn.make "sum"
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
                            a <- Fn.receive $ Fn.in_ "a"  -- TODO: some operator i.e. <<+ "a"
                            b <- Fn.receive $ Fn.in_ "b"  -- TODO: some operator i.e. <<+ "b"
                            Fn.send (Fn.out_ "sum") $ a + b  -- TODO: some operator i.e. +>> "b"
            maybeNode <- toolkit # Toolkit.spawn "sum" # liftEffect -- or `spawnAndRun`
            case maybeNode of
                Just node -> liftEffect $ do -- do inside `NodeM` ?
                    Node.run' $ Node.imapState Tuple.snd ((/\) unit) $ node -- FIXME: may be no need in `imapState` and joining it with patch state
                    Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
                    Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
                    sum <- Node.getO node (Fn.out_ "sum") -- TODO: some operator i.e. v <- "sum" <+ node
                    shouldEqual sum 8
                Nothing ->
                    fail "node wasn't spawned"

            pure unit

        it "spawning with state works" $ do
            let
                intChan = Ch.hot "int" 0
                toolkit :: Toolkit Unit Int
                toolkit =
                    Toolkit.registerFn (Toolkit.empty "Ints" unit 0) "---"
                        $ Fn.make "sum"
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
                            State.modify_ (map $ const $ show $ a - b)
                            Fn.send (Fn.out_ "sum") $ a + b
            maybeNode <- toolkit # Toolkit.spawn "sum" # liftEffect
            case maybeNode of
                Just node -> liftEffect $ do -- do inside `NodeM` ?
                    Console.log $ show "before everything"
                    stateSig <- Node.run "---" $ Node.imapState Tuple.snd ((/\) unit) $ node
                    stateAtStart <- Signal.get stateSig
                    stateAtStart `shouldEqual` "0"
                    Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
                    stateAfterFirstInlet <- Signal.get stateSig
                    stateAfterFirstInlet `shouldEqual` "5"
                    Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
                    finalState <- Signal.get stateSig
                    finalState `shouldEqual` "2"
                Nothing ->
                    fail "node wasn't spawned"

            pure unit

    describe "bar" $ do
        pure unit
