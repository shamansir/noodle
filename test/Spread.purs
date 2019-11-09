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
        let from' = toNumber (toCharCode from)
            to' = toNumber (toCharCode to)
        in MyChar <$> (fromCharCode $ floor $ from' + pos * (to' - from'))


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

    it "index is wrapped when it exceeds count" $ do
        let spread = Spread.make (0.0 /\ 3.5) 8
        Just 0.0 `shouldEqual` (spread !! 8)
        Just 1.5 `shouldEqual` (spread !! 11)
        Just 1.5 `shouldEqual` (spread !! 19)

    it "negative index is to count from end" $ do
        let spread = Spread.make (0.0 /\ 3.5) 8
        Just 2.5 `shouldEqual` (spread !! -3)

    it "negative index is to count from end, even after \"overflow\"" $ do
        let spread = Spread.make (0.0 /\ 3.5) 8
        Just 2.5 `shouldEqual` (spread !! -11)

    it "zero-sized spread is always empty" $ do
        let spread = Spread.make (0.1 /\ 3.5) 0
        [ ] `shouldEqual` (catMaybes $ Spread.run spread)
        Nothing `shouldEqual` (spread !! 5)

    it "creating singleton spread" $ do
        let spread = Spread.singleton unit
        [ unit
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just unit `shouldEqual` (spread !! 0)

    it "one-element spread for numbers is not NaN" $ do
        let spread = Spread.make (0.1 /\ 3.5) 1
        [ 0.1
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just 0.1 `shouldEqual` (spread !! 0)

    it "backwards spread is when count is negative" $ do
        let spread = Spread.make (0.0 /\ 3.5) (-8)
        [ 3.5
        , 3.0
        , 2.5
        , 2.0
        , 1.5
        , 1.0
        , 0.5
        , 0.0
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just 2.0 `shouldEqual` (spread !! 3)

    it "works for custom types" $ do
        let spread = Spread.make (MyChar 'A' /\ MyChar 'F') 6
        [ MyChar 'A'
        , MyChar 'B'
        , MyChar 'C'
        , MyChar 'D'
        , MyChar 'E'
        , MyChar 'F'
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just (MyChar 'C') `shouldEqual` (spread !! 2)

    it "joins spreads properly" $ do
        let
            spread1 =
                Spread.join
                    (Spread.make (0.0 /\ 4.0) 6)
                    (Spread.make (MyChar 'A' /\ MyChar 'F') 2)
        [ 0.0 /\ MyChar 'A'
        , 0.8 /\ MyChar 'F'
        , 1.6 /\ MyChar 'A'
        , 2.4 /\ MyChar 'F'
        , 3.2 /\ MyChar 'A'
        , 4.0 /\ MyChar 'F'
        ] `shouldEqual` (catMaybes $ Spread.run spread1)
        Just (1.6 /\ MyChar 'A') `shouldEqual` (spread1 !! 2)

        let
            spread2 =
                Spread.join
                    (Spread.make (0.0 /\ 3.0) 2)
                    (Spread.make (MyChar 'A' /\ MyChar 'F') 6)
        [ 0.0 /\ MyChar 'A'
        , 3.0 /\ MyChar 'B'
        , 0.0 /\ MyChar 'C'
        , 3.0 /\ MyChar 'D'
        , 0.0 /\ MyChar 'E'
        , 3.0 /\ MyChar 'F'
        ] `shouldEqual` (catMaybes $ Spread.run spread2)
        Just (3.0 /\ MyChar 'D') `shouldEqual` (spread2 !! 3)

    it "repeating works" $ do
        let spread = Spread.repeat 5 $ MyChar 'F'
        [ MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        ] `shouldEqual` (catMaybes $ Spread.run spread)
        Just (MyChar 'F') `shouldEqual` (spread !! 2)

    it "concatenating works" $ do
        let
            spreadA = Spread.repeat 5 $ MyChar 'F'
            spreadB = Spread.repeat 3 $ MyChar 'A'
            spreadC = Spread.concat spreadA spreadB
        [ MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        , MyChar 'F'
        , MyChar 'A'
        , MyChar 'A'
        , MyChar 'A'
        ] `shouldEqual` (catMaybes $ Spread.run spreadC)
        Just (MyChar 'F') `shouldEqual` (spreadC !! 2)
        Just (MyChar 'A') `shouldEqual` (spreadC !! 5)
