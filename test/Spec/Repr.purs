module Test.Spec.Repr where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Symbol (reflectSymbol)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (accept) as ViC

import Noodle.Raw.FromToRec as RR

import Example.Toolkit.Minimal.Repr (MinimalVRepr(..))


spec :: Spec Unit
spec = do

    describe "creating maps from reprs" $ do

        it "converting to repr works" $ do
            let map = RR.fromRec reflectSymbol { a : 5, b : 3 }
            (Map.lookup "a" map) `shouldEqual` (Just $ ViC.accept $ Int 5)
            (Map.lookup "b" map) `shouldEqual` (Just $ ViC.accept $ Int 3)

        it "converting to repr works with different types" $ do
            let map = RR.fromRec reflectSymbol { a : 5, b : "3" }
            (Map.lookup "a" map) `shouldEqual` (Just $ ViC.accept $ Int 5)
            (Map.lookup "b" map) `shouldEqual` (Just $ ViC.accept $ Str "3")

        it "converting from repr works" $ do
            let
                (rec :: Record ( a :: Int, b :: Int )) =
                    RR.toRec identity
                        $ Map.insert "a" (ViC.accept $ Int 5)
                        $ Map.insert "b" (ViC.accept $ Int 3)
                        $ Map.empty
            rec.a `shouldEqual` 5
            rec.b `shouldEqual` 3

        it "converting from repr works with different types" $ do
            let
                (rec :: Record ( a :: Int, b :: String )) =
                    RR.toRec identity
                        $ Map.insert "a" (ViC.accept $ Int 5)
                        $ Map.insert "b" (ViC.accept $ Str "3")
                        $ Map.empty
            rec.a `shouldEqual` 5
            rec.b `shouldEqual` "3"