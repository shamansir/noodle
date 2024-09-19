module Test.Spec.Toolkit where

import Prelude

import Effect.Class (liftEffect)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)

import Data.Tuple.Nested ((/\))
import Data.Symbol (class IsSymbol)

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

import Noodle.Id (familyR, family, FamilyR(..)) as Id
import Noodle.Node (run, _listenUpdatesAndRun, state, modifyState) as Node
import Noodle.Node ((#->), (@->), (<=@))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, spawn, register, registerRaw, mapFamilies, mapRawFamilies) as Toolkit
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.Family (familyIdOf) as Family
import Noodle.Toolkit.Families (Families, F)
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (familyIdOf) as RawFamily

import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Node.Stateful as Stateful
import Test.MyToolkit.Node.Raw.Concat as RawConcat
import Test.MyToolkit.Node.Raw.Sum as RawSum
import Test.MyToolkit.Node.Raw.Stateful as RawStateful
import Test.MyToolkit.Toolkit (Toolkit, toolkit) as My


spec :: Spec Unit
spec = do

    describe "registering" $ do

        it "registers family" $ do
            let
                (_ :: Toolkit (Concat.F :> TNil) _ _) =
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                (_ :: Toolkit (Sum.F :> Concat.F :> TNil) _ _) =
                    Toolkit.empty "test"
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
                    Toolkit.empty "test"
                        # Toolkit.register Concat.family
                        # Toolkit.spawn Concat._concat
            concatNode # Node.run
            atOut <- concatNode <=@ _.out
            atOut `shouldEqual` ""


    describe "iterating through families" $ do

        let
            familyToString :: forall f state is os repr m. IsSymbol f => Family f state is os repr m -> String
            familyToString = Family.familyIdOf >>> Id.familyR >>> Id.family
            rawFamilyToString :: forall repr m. Raw.Family repr m -> String
            rawFamilyToString = RawFamily.familyIdOf >>> \(Id.FamilyR { family }) -> family

        it "it is possible to iterate through all typed families" $ liftEffect $ do
            let emptyTkArray =
                    Toolkit.empty "test"
                        # Toolkit.mapFamilies familyToString
            emptyTkArray `shouldEqual` []
            let nonEmptyTkArray =
                    Toolkit.empty "test-2"
                        # Toolkit.register Concat.family
                        # Toolkit.register Sum.family
                        # Toolkit.register Stateful.family
                        # Toolkit.mapFamilies familyToString
            nonEmptyTkArray `shouldEqual` [ "stateful", "sum", "concat" ] -- since we use `#` operator, `Put` typeclass pushes "stateful" before "sum" & s.o.

        it "it is possible to iterate through all raw families" $ liftEffect $ do
            let emptyTkArray =
                    Toolkit.empty "test"
                        # Toolkit.mapRawFamilies rawFamilyToString
            emptyTkArray `shouldEqual` []
            let nonEmptyTkArray =
                    Toolkit.empty "test-2"
                        # Toolkit.registerRaw RawConcat.family
                        # Toolkit.registerRaw RawSum.family
                        # Toolkit.registerRaw RawStateful.family
                        # Toolkit.mapRawFamilies rawFamilyToString
            nonEmptyTkArray `shouldEqual` [ "concatR", "statefulR", "sumR" ] -- raw families are sorted alphabetically