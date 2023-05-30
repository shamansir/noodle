module Data.Unfoldable.Extra where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (swap) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Data.Unfoldable (class Unfoldable, unfoldr)


iterateFrom :: forall f a b. Unfoldable f => (a -> Maybe (a /\ b)) -> a -> Int -> f b
iterateFrom produce start n =
    unfoldr go $ start /\ (n - 1)
    where
    go (x /\ n') =
        if n' > 0 then
            map (flip (/\) $ n' - 1) <$> Tuple.swap <$> produce x
        else Nothing
