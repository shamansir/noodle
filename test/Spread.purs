module RpdTest.Spread
    ( spec
    ) where

import Prelude

import Effect.Class (liftEffect)

import Data.Lerp (class Lerp)
import Data.Spread (Spread, (!!))
import Data.Spread as Spread

import Data.Int (toNumber, floor)
import Data.Char (fromCharCode, toCharCode)
import Data.Array (catMaybes)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


newtype MyChar = MyChar Char


-- sample :: String
-- sample = "SATORAREPOTENETOPERAROTAS"


instance showMyChar :: Show MyChar where
    show (MyChar c) = show c


instance eqMyChar :: Eq MyChar where
    eq (MyChar a) (MyChar b) = eq a b


instance lerpMyChar :: Lerp MyChar where
    lerp (MyChar from /\ MyChar to) pos =
        MyChar <$> (fromCharCode $ floor (toNumber (toCharCode from) + pos * toNumber ((toCharCode to - toCharCode from))))


spec :: Spec Unit
spec =
  describe "Spread generation" $ do

    it "works for numbers" $ do
        let spread = Spread.make (0.0 /\ 3.5) 8
        [ 0.0
        , 0.5
        , 1.0
        , 1.5
        , 2.0
        , 2.5
        , 3.0
        , 3.5
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just 1.5 `shouldEqual` (spread !! 3)

    it "works for custom types" $ do
        let spread = Spread.make (MyChar 'A' /\ MyChar 'F') 6
        [ 'A'
        , 'B'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        ] `shouldEqual` ((\(MyChar c) -> c) <$> (catMaybes $ Spread.run spread))
        Just (MyChar 'C') `shouldEqual` (spread !! 2)

    it "joins spreads properly" $ do
        let
            spread1 =
                Spread.join
                    (Spread.make (0.0 /\ 4.0) 6)
                    (Spread.make (MyChar 'A' /\ MyChar 'F') 2)
        [ 0.0 /\ 'A'
        , 0.8 /\ 'F'
        , 1.6 /\ 'A'
        , 2.4 /\ 'F'
        , 3.2 /\ 'A'
        , 4.0 /\ 'F'
        ] `shouldEqual` ((\(n /\ MyChar c) -> n /\ c) <$> (catMaybes $ Spread.run spread1))
        Just (1.6 /\ MyChar 'A') `shouldEqual` (spread1 !! 2)

        let
            spread2 =
                Spread.join
                    (Spread.make (0.0 /\ 3.0) 2)
                    (Spread.make (MyChar 'A' /\ MyChar 'F') 6)
        [ 0.0 /\ 'A'
        , 3.0 /\ 'B'
        , 0.0 /\ 'C'
        , 3.0 /\ 'D'
        , 0.0 /\ 'E'
        , 3.0 /\ 'F'
        ] `shouldEqual` ((\(n /\ MyChar c) -> n /\ c) <$> (catMaybes $ Spread.run spread2))
        Just (3.0 /\ MyChar 'D') `shouldEqual` (spread2 !! 3)
