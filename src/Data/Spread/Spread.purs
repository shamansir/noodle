module Data.Spread where

import Prelude

import Data.Lerp (class Lerp)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


data Spread x = Spread Int (Int -> Maybe x)


make :: forall x. Lerp x => x /\ x -> Int -> Spread x
make (from /\ to) count =
    Spread 0 $ const Nothing
