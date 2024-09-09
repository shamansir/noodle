module Test.Spec.Toolkit where

import Prelude

import Effect.Class (liftEffect)

import Data.Tuple.Nested ((/\))

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)


import Noodle.Node (run, _listenUpdatesAndRun, state, modifyState) as Node
import Noodle.Node ((#->), (@->), (<=@))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, spawn, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, FNil, type (//))

import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Node.Stateful as Stateful
import Test.MyToolkit.Toolkit (Toolkit, toolkit) as My


spec :: Spec Unit
spec = do

    describe "registering" $ do

        it "registers family" $ do
            let
                (_ :: Toolkit (Concat.F // FNil) _ _) =
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                (_ :: Toolkit (Sum.F // Concat.F // FNil) _ _) =
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                        # Toolkit.register (Sum.family Sum.sumBoth)
            pure unit


    describe "spawning" $ do

        it "spawning node from a family" $ liftEffect $ do
            (concatNode :: Concat.Node) <- Toolkit.spawn Concat._concat My.toolkit
            concatNode # Node.run
            atOut <- concatNode <=@ _.out
            atOut `shouldEqual` ""

        it "spawning node from a family and operating with it" $ liftEffect $ do
            (concatNode :: Concat.Node) <- Toolkit.spawn Concat._concat My.toolkit
            concatNode # Node._listenUpdatesAndRun
            _ <- concatNode #-> Concat.left_in  /\ "foo"
            _ <- concatNode #-> Concat.right_in /\ "bar"
            -- _ <- concatNode @-> Concat.len_out /\ 7
            atOut <- concatNode <=@ _.out
            atLen <- concatNode <=@ _.len
            atOut `shouldEqual` "foobar"
            atLen `shouldEqual` 6

        it "spawning node with a state from a family" $ liftEffect $ do
            (statefulNode :: Stateful.Node) <- Toolkit.spawn Stateful._stateful My.toolkit
            statefulNode # Node._listenUpdatesAndRun
            _ <- statefulNode #-> Stateful.a_in /\ 5
            _ <- statefulNode #-> Stateful.b_in /\ 7
            state <- Node.state statefulNode
            state `shouldEqual` "x-0-0-5-12"


    describe "registering & spawning" $ do

        it "it is possible to register family and immediately spawn the node that belongs to it" $ liftEffect $ do
            (concatNode :: Concat.Node) <-
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                        # Toolkit.spawn Concat._concat
            concatNode # Node.run
            atOut <- concatNode <=@ _.out
            atOut `shouldEqual` ""