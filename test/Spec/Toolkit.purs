module Test.Spec.Toolkit where

import Prelude

import Effect.Class (liftEffect)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)

import Data.Tuple.Nested ((/\))
import Data.Symbol (class IsSymbol)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Id (familyR, family, toolkitR) as Id
import Noodle.Node (_listenUpdatesAndRun, run, state) as Node
import Noodle.Node ((#->), (@->), (<=@))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, spawn, register, registerRaw, mapFamilies, mapRawFamilies) as Toolkit
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.Family (familyIdOf) as Family
import Noodle.Toolkit.Families (Families, F)
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (familyIdOf) as RawFamily

import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Stateful as Stateful
import Example.Toolkit.Minimal.Node.ModifiesPatch as ModifiesPatch
import Example.Toolkit.Minimal.Node.Raw.Concat as RawConcat
import Example.Toolkit.Minimal.Node.Raw.Sum as RawSum
import Example.Toolkit.Minimal.Node.Raw.Stateful as RawStateful
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)
import Example.Toolkit.Minimal.StRepr (MinimalStRepr)
import Example.Toolkit.Minimal.Toolkit (Toolkit, toolkit, MINIMAL,  minimalTk) as My


type MinimalTk fs m = Toolkit My.MINIMAL fs MinimalStRepr MinimalVRepr m


spec :: Spec Unit
spec = do

    describe "registering" $ do

        it "registers family" $ do
            let
                (_ :: MinimalTk (Concat.F :> TNil) _) =
                    Toolkit.empty My.minimalTk (Id.toolkitR "Test")
                        # Toolkit.register Concat.family
                (_ :: MinimalTk (Sum.F :> Concat.F :> TNil) _) =
                    Toolkit.empty My.minimalTk (Id.toolkitR "Test")
                        # Toolkit.register Concat.family
                        # Toolkit.register Sum.family
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
                    (Toolkit.empty My.minimalTk (Id.toolkitR "test") :: MinimalTk _ _)
                        # Toolkit.register Concat.family
                        # Toolkit.spawn Concat._concat
            concatNode # Node.run
            atOut <- concatNode <=@ _.out
            atOut `shouldEqual` ""

    describe "iterating through families" $ do

        let
            familyToString :: forall f state is os repr m. IsSymbol f => Family f state is os repr m -> String
            familyToString = Family.familyIdOf >>> Id.familyR >>> Id.family
            rawFamilyToString :: forall strepr chrepr m. Raw.Family strepr chrepr m -> String
            rawFamilyToString = RawFamily.familyIdOf >>> \familyR -> Id.family familyR

        it "it is possible to iterate through all typed families" $ liftEffect $ do
            let emptyTkArray =
                    Toolkit.empty My.minimalTk (Id.toolkitR "test")
                        # Toolkit.mapFamilies familyToString
            emptyTkArray `shouldEqual` []
            let nonEmptyTkArray =
                    (Toolkit.empty My.minimalTk (Id.toolkitR "test-2") :: MinimalTk _ _)
                        # Toolkit.register ModifiesPatch.family
                        # Toolkit.register Concat.family
                        # Toolkit.register Sum.family
                        # Toolkit.register Stateful.family
                        # Toolkit.mapFamilies familyToString
            nonEmptyTkArray `shouldEqual` [ "stateful", "sum", "concat", "modifiesPatch" ] -- since we use `#` operator, `Put` typeclass pushes "stateful" before "sum" & s.o.

        it "it is possible to iterate through all raw families" $ liftEffect $ do
            let emptyTkArray =
                    (Toolkit.empty My.minimalTk (Id.toolkitR "test") :: MinimalTk _ _ )
                        # Toolkit.mapRawFamilies rawFamilyToString
            emptyTkArray `shouldEqual` []
            let nonEmptyTkArray =
                    Toolkit.empty My.minimalTk (Id.toolkitR "test-2")
                        # Toolkit.registerRaw RawConcat.family
                        # Toolkit.registerRaw RawSum.family
                        # Toolkit.registerRaw RawStateful.family
                        # Toolkit.mapRawFamilies rawFamilyToString
            nonEmptyTkArray `shouldEqual` [ "concatR", "statefulR", "sumR" ] -- raw families are sorted alphabetically