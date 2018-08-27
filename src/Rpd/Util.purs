module Rpd.Util
    ( convertKeysInMap
    , convertMap
    , type (/->)
    ) where

import Prelude (class Ord, map, (>>>))

import Data.Map (Map)
import Data.Map as Map
import Data.Bifunctor (lmap, bimap)

infixr 6 type Map as /->

convertKeysInMap
    :: forall k k' v
     . Ord k => Ord k'
    => (k -> k')
    -> (k /-> v)
    -> (k' /-> v)
convertKeysInMap toNewKey =
    Map.toUnfoldable >>> amap (lmap toNewKey) >>> Map.fromFoldable where
        amap :: forall a b. (a -> b) -> (Array a -> Array b)
        amap = map

convertMap
    :: forall k k' v v'
     . Ord k => Ord k'
    => (k -> k')
    -> (v -> v')
    -> (k /-> v)
    -> (k' /-> v')
convertMap toNewKey toNewVal =
    Map.toUnfoldable >>> amap (bimap toNewKey toNewVal) >>> Map.fromFoldable where
        amap :: forall a b. (a -> b) -> (Array a -> Array b)
        amap = map
