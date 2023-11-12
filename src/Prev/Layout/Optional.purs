module Prev.Layout.Optional where


import Prelude
import Prev.Web.Layout as L


import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Pos, Size)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.List (List)
import Data.List as List


extractMaybe :: forall a x z. (Maybe a /\ x /\ z)  -> Maybe (a /\ x /\ z)
extractMaybe (Nothing /\ _ /\ _) = Nothing
extractMaybe (Just v /\ pos' /\ size) = Just $ v /\ pos' /\ size


abandon :: forall l a. Functor l => Eq a => a -> l (Maybe a) -> l (Maybe a) -- Maybe (l (Maybe a))
abandon needle  = (<$>) freeIfEq
    where
        freeIfEq (Just v) | v == needle = Nothing
        freeIfEq (Just v) | otherwise = Just v
        freeIfEq Nothing = Nothing



fold :: forall a l k. L.IsLayout l => ((a /\ Pos /\ Size) -> k -> k) -> k -> l (Maybe a) -> k
fold f = L.fold skipNothings
    where
        skipNothings (Nothing /\ _ /\ _) k = k
        skipNothings (Just v /\ pos /\ size) k = f (v /\ pos /\ size) k


find :: forall l a. L.IsLayout l => Eq a => a -> l (Maybe a) -> Maybe (Pos /\ Size)
find a = L.find $ Just a


sample :: forall l a. L.IsLayout l => Pos -> l (Maybe a) -> Maybe (a /\ Pos /\ Size)
sample pos = L.sample pos >>> ((=<<) extractMaybe)


atPos :: forall l a. L.IsLayout l => Pos -> l (Maybe a) -> Maybe a
atPos pos = L.atPos pos >>> ((=<<) identity)


atPos' :: forall l a. L.IsLayout l => Pos -> l (Maybe a) -> Maybe (a /\ Pos)
atPos' pos l =
    case L.atPos' pos l of
        (Just (Just v /\ pos')) -> Just (v /\ pos')
        _ -> Nothing


atPos'' :: forall l a. L.IsLayout l => Pos -> l (Maybe a) -> Maybe (a /\ Size)
atPos'' pos l =
    case L.atPos'' pos l of
        (Just (Just v /\ size)) -> Just (v /\ size)
        _ -> Nothing


pack :: forall l a. L.IsAutoLayout l => a -> Size -> l (Maybe a) -> Maybe (l (Maybe a))
pack = L.pack <<< Just


packOrDrop :: forall l a. L.IsAutoLayout l => a -> Size -> l (Maybe a) -> l (Maybe a)
packOrDrop = L.packOrDrop <<< Just


toArray :: forall l a. L.IsLayout l => l (Maybe a) -> Array (a /\ Pos /\ Size)
toArray = L.toArray >>> (<$>) extractMaybe >>> Array.catMaybes


toList :: forall l a. L.IsLayout l => l (Maybe a) -> List (a /\ Pos /\ Size)
toList = L.toList >>> (<$>) extractMaybe >>> List.catMaybes


count :: forall l a. L.IsLayout l => l (Maybe a) -> Int
count = toArray >>> Array.length