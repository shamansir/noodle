module Data.Spread where

import Prelude

import Data.Lerp (class Lerp, lerp)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array (range, catMaybes)
import Data.Tuple.Nested ((/\), type (/\))


data Spread a = Spread Int (Int -> Maybe a)


-- TODO: Functor etc.


infixl 8 get as !!


nil :: forall a. Spread a
nil = Spread 0 $ const Nothing


get :: forall a. Spread a -> Int -> Maybe a
get (Spread _ f) idx = f idx


make :: forall x. Lerp x => x /\ x -> Int -> Spread x
make range count =
    Spread count \idx ->
        lerp range $ toNumber (idx `mod` count) / toNumber (count - 1)


run :: forall x. Spread x -> Array (Maybe x)
run (Spread count f) = f <$> range 0 (count - 1)


join :: forall a b. Spread a -> Spread b -> Spread (a /\ b)
join (Spread countA fA) (Spread countB fB) =
    Spread (max countA countB) \idx -> (/\) <$> fA idx <*> fB idx
