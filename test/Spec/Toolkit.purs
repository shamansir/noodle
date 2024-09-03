module Test.Spec.Toolkit where

import Prelude

import Effect.Class (liftEffect)

import Data.Tuple.Nested ((/\))

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)


import Noodle.Node (run, listenUpdatesAndRun) as Node
import Noodle.Node ((#->), (<=@))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, spawn, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, FNil, type (//))

import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Toolkit (Toolkit, toolkit) as My


spec :: Spec Unit
spec = do

    describe "registering" $ do

        it "registers family" $ do
            let
                tk :: Toolkit (Concat.F // FNil) _ _
                tk =
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                tk2 :: Toolkit (Sum.F // Concat.F // FNil) _ _
                tk2 = Toolkit.empty "test"
                        # Toolkit.register Concat.family
                        # Toolkit.register (Sum.family Sum.sumBoth)
            pure unit


    describe "spawning" $ do

        it "spawning node from a family" $ liftEffect $ do
            (concatNode :: Concat.Node) <- Toolkit.spawn Concat._concat My.toolkit
            concatNode # Node.run
            atOut <- concatNode <=@ _.out
            atOut `shouldEqual` ""

        it "spawning node from a family (2)" $ liftEffect $ do
            (concatNode :: Concat.Node) <- Toolkit.spawn Concat._concat My.toolkit
            concatNode # Node.listenUpdatesAndRun
            _ <- concatNode #-> Concat.left_in  /\ "foo"
            _ <- concatNode #-> Concat.right_in /\ "bar"
            atOut <- concatNode <=@ _.out
            atLen <- concatNode <=@ _.len
            atOut `shouldEqual` "foobar"
            atLen `shouldEqual` 6


    describe "registering & spawning" $ do

        it "it is possible to register family and immediately spawn the node that belongs to it" $ do
            pure unit