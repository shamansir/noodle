module Data.Array.Extra where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Array (sortWith, elemIndex, mapWithIndex, length) as Array
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))



-- | Sort one array using another array of mathing things as an order reference, if the matching item wasn't found in the second array, it goes in the end of the resulting array.
-- | Takes a lot of time ~ O(n^2), because it searches for a matching element in the second array for every element in the first one.
-- TODO: `Array b` could be required to be non-empty array
sortUsing :: forall a b. Eq b => (a -> b) -> Array b -> Array a -> Array a
sortUsing f arrB =
    Array.mapWithIndex ((/\))
    >>> Array.sortWith
        (\(aIdx /\ a) ->
            fromMaybe
                (Array.length arrB + aIdx) -- when matching item wasn't found in B, put it further than index in B could ever be, using its original index in A as a distance
                $ Array.elemIndex (f a) arrB
        )
    >>> map Tuple.snd