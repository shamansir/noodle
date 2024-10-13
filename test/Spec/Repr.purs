module Test.Spec.Repr where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Symbol (reflectSymbol)

import Noodle.Raw.FromToRec as RR

import Example.Toolkit.Minimal.Repr (ISRepr(..))


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

    describe "creating maps from reprs" $ do

        it "converting to repr works" $ do
            let map = RR.fromRec reflectSymbol { a : 5, b : 3 }
            Map.lookup "a" map `shouldEqual` (Just $ Int 5)
            Map.lookup "b" map `shouldEqual` (Just $ Int 3)

        it "converting to repr works with different types" $ do
            let map = RR.fromRec reflectSymbol { a : 5, b : "3" }
            Map.lookup "a" map `shouldEqual` (Just $ Int 5)
            Map.lookup "b" map `shouldEqual` (Just $ Str "3")

        it "converting from repr works" $ do
            let
                (rec :: Record ( a :: Int, b :: Int )) =
                    RR.toRec identity
                        $ Map.insert "a" (Int 5)
                        $ Map.insert "b" (Int 3)
                        $ Map.empty
            rec.a `shouldEqual` 5
            rec.b `shouldEqual` 3

        it "converting from repr works with different types" $ do
            let
                (rec :: Record ( a :: Int, b :: String )) =
                    RR.toRec identity
                        $ Map.insert "a" (Int 5)
                        $ Map.insert "b" (Str "3")
                        $ Map.empty
            rec.a `shouldEqual` 5
            rec.b `shouldEqual` "3"


{-
sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }
-}