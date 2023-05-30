module Data.Unfoldable1.Extra where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Enum (class Enum, succ)
import Data.Tuple.Nested ((/\))

import Data.Unfoldable1 (class Unfoldable1, unfoldr1)


-- FIXME: reimplemented from the latest `unfoldable`
iterateN :: forall f a. Unfoldable1 f => (a -> a) -> a -> Int -> f a
iterateN f =
    iterateN' (Just <<< f)


iterateN' :: forall f a. Unfoldable1 f => (a -> Maybe a) -> a -> Int -> f a
iterateN' produce start n = unfoldr1 go $ start /\ (n - 1)
  where
  go (x /\ n') = x /\
    if n' > 0 then
        (flip (/\) $ n' - 1) <$> produce x
    else Nothing


iterateEnum :: forall f a. Unfoldable1 f => Bounded a => Enum a => Int -> f a
iterateEnum =
    iterateN' succ bottom
