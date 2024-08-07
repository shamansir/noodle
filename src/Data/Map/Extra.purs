module Data.Map.Extra
    ( type (/->)
    , lookupBy
    , lookupBy'
    , stringifyKeys
    ) where

import Prelude ((>>>), (<<<), class Ord, class Eq, (==))


import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.List as List
import Data.FoldableWithIndex (foldrWithIndex)


infixr 6 type Map as /->



-- lookupBy :: forall k v. Ord k => (k -> Boolean) -> (k /-> v) -> Maybe v
lookupBy :: forall k v. Ord k => (k -> Boolean) -> Map k v -> Maybe v
lookupBy f = Map.filterKeys f >>> Map.values >>> List.head


lookupBy' :: forall a k v. Ord k => Eq a => (k -> a) -> a -> Map k v -> Maybe v
lookupBy' f sample = lookupBy (f >>> (==) sample)


stringifyKeys :: forall k v. (k -> String) -> Map k v -> Map String v
stringifyKeys f = foldrWithIndex (Map.insert <<< f) Map.empty