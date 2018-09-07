module Rpd.Util
    ( convertKeysInMap
    , convertMap
    , type (/->)
    , Subscriber, Canceler
    , Flow, PushF, PushableFlow(..)
    , flow
    ) where

import Prelude (class Ord, map, (>>>), Unit, identity)

import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Bifunctor (lmap, bimap)

import FRP.Event (Event)

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


type Canceler =
    Effect Unit
type Subscriber =
    Effect Canceler


type Flow d = Event d
type PushF d = (d -> Effect Unit)
data PushableFlow d = PushableFlow (PushF d) (Event d)


flow :: forall d. Event d -> Flow d
flow = identity
