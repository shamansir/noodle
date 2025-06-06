module Data.Map.Extra
    ( type (/->)
    , update'
    , lookupBy
    , lookupBy'
    , lookupKey
    , stringifyKeys
    , mapKeys
    , mapKeysMaybe
    , join
    , joinWith
    , withKeys
    , withKeys'
    ) where


import Prelude


import Data.FoldableWithIndex (foldrWithIndex, foldlWithIndex)
import Data.Traversable (sequence)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
-- import Data.Monoid (mempty)
import Data.List as List
import Data.Array as Array
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple (fst, snd) as Tuple


infixr 6 type Map as /->


update' :: forall k v. Ord k => (v -> v) -> k -> Map k v -> Map k v
update' f = Map.update $ Just <<< f


lookupBy :: forall k v. Ord k => (k -> Boolean) -> Map k v -> Maybe v
lookupBy f = Map.filterKeys f >>> Map.values >>> List.head


lookupBy' :: forall a k v. Ord k => Eq a => (k -> a) -> a -> Map k v -> Maybe v
lookupBy' f sample = lookupBy (f >>> (==) sample)


lookupKey :: forall k v. Eq v => v -> Map k v -> Maybe k
lookupKey v = Map.toUnfoldable >>> Array.find (Tuple.snd >>> (_ == v)) >>> map Tuple.fst
-- FIXME: may be there's a faster way without conversion to Array, i.e. filterWithKey ...


stringifyKeys :: forall k v. (k -> String) -> Map k v -> Map String v
stringifyKeys f = foldrWithIndex (Map.insert <<< f) Map.empty


-- below, from https://raw.githubusercontent.com/colehaus/purescript-probability/refs/tags/v5.2.0/src/Data/Map/Extras.purs


mapKeys :: forall j k v. Ord j => Ord k => (k -> j) -> Map k v -> Map j v
mapKeys f = mapKeysMaybeWithValueWith (Just <<< f) (\k v _ -> Just (Tuple (f k) v))

mapKeysMaybe :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> Map k v -> Map j v
mapKeysMaybe f = mapKeysMaybeWithValueWith f (\k v _ -> (_ `Tuple` v) <$> f k)


withKeys :: forall k a b. Ord k => (k -> b) -> Map k a -> Map k b
withKeys f = Map.mapMaybeWithKey (const <<< Just <<< f)


withKeys' :: forall f k a b. Applicative f => Ord k => (k -> f b) -> Map k a -> f (Map k b)
withKeys' f = sequence <<< Map.mapMaybeWithKey (const <<< Just <<< f)


mapKeysMaybeWithValueWith :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> (k -> v -> Maybe v -> Maybe (Tuple j v)) -> Map k v -> Map j v
mapKeysMaybeWithValueWith f g = foldlWithIndex h Map.empty
  where
    h k acc v = maybe acc (flip (uncurry Map.insert) acc) $ g k v <<< (flip Map.lookup acc) =<< f k


join :: forall k a b. Ord k => Map k a -> Map k b -> Map k (Tuple a b)
join = joinWith Tuple


joinWith :: forall k a b c. Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
joinWith f mapA = foldlWithIndex h Map.empty
  where h k acc b = maybe acc (\a -> Map.insert k (f a b) acc) $ Map.lookup k mapA