module Data.Map.Extra
    ( type (/->)
    , lookupBy
    , lookupBy'
    , stringifyKeys
    , mapKeys
    , mapMaybe
    , mapKeysMaybe
    ) where


import Prelude


import Data.FoldableWithIndex (foldrWithIndex, foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
-- import Data.Monoid (mempty)
import Data.List as List
import Data.Tuple (Tuple(..), uncurry)


infixr 6 type Map as /->



-- lookupBy :: forall k v. Ord k => (k -> Boolean) -> (k /-> v) -> Maybe v
lookupBy :: forall k v. Ord k => (k -> Boolean) -> Map k v -> Maybe v
lookupBy f = Map.filterKeys f >>> Map.values >>> List.head


lookupBy' :: forall a k v. Ord k => Eq a => (k -> a) -> a -> Map k v -> Maybe v
lookupBy' f sample = lookupBy (f >>> (==) sample)


stringifyKeys :: forall k v. (k -> String) -> Map k v -> Map String v
stringifyKeys f = foldrWithIndex (Map.insert <<< f) Map.empty


-- below, from https://raw.githubusercontent.com/colehaus/purescript-probability/refs/tags/v5.2.0/src/Data/Map/Extras.purs


mapMaybe :: forall k v u. Ord k => (v -> Maybe u) -> Map k v -> Map k u
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: forall k v u. Ord k => (k -> v -> Maybe u) -> Map k v -> Map k u
mapMaybeWithKey f m = foldlWithIndex f' Map.empty m
  where
    f' k acc v = maybe acc (flip (Map.insert k) acc) $ f k v

mapKeys :: forall j k v. Ord j => Ord k => (k -> j) -> Map k v -> Map j v
mapKeys f = mapKeysMaybeWithValueWith (Just <<< f) (\k v _ -> Just (Tuple (f k) v))

mapKeysMaybe :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> Map k v -> Map j v
mapKeysMaybe f = mapKeysMaybeWithValueWith f (\k v _ -> (_ `Tuple` v) <$> f k)

-- | The value at the greater of the two original keys is used as the first argument to c.
{-
mapKeysWith :: forall j k v. Ord j => Ord k => (v -> v -> v) -> (k -> j) -> Map k v -> Map j v
mapKeysWith f g = mapKeysMaybeWithValueWith (Just <<< g) h
  where
    h k v (Just v') = Just (Tuple (g k) (f v v'))
    h k v Nothing = Just (Tuple (g k) v)
-}

mapKeysMaybeWithValueWith :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> (k -> v -> Maybe v -> Maybe (Tuple j v)) -> Map k v -> Map j v
mapKeysMaybeWithValueWith f g m = foldlWithIndex h Map.empty m
  where
    h k acc v = maybe acc (flip (uncurry Map.insert) acc) $ g k v <<< (flip Map.lookup acc) =<< f k
