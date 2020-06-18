module Noodle.Util
    ( convertKeysInMap
    , convertMap
    , type (/->)
    , Subscriber, Canceler
    , Flow, PushF, PushableFlow(..)
    , flow, never
    , Position, Rect, Bounds, quickBounds, quickBounds'
    ) where


import Prelude

import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (class Foldable, foldr)
import Data.Bifunctor (lmap, bimap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))

import FRP.Event (Event, create)


infixr 6 type Map as /->


type Position = { x :: Number, y :: Number }
type Rect = { width :: Number, height :: Number }
type Bounds = Position /\ Rect


-- instance ringPosition :: Ring Position where
--     sub posA posB =
--       { x : posA.x + posB.x, y : posA.y + posB.y }


quickBounds :: Number -> Number -> Number -> Number -> Bounds
quickBounds x y width height =
    -- { pos : { x, y }, rect : { width, height }}
    { x, y } /\ { width, height }


quickBounds' :: Number /\ Number /\ Number /\ Number -> Bounds
quickBounds' (x /\ y /\ width /\ height) =
    -- { pos : { x, y }, rect : { width, height }}
    { x, y } /\ { width, height }


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
data PushableFlow d = PushableFlow (PushF d) (Flow d)


flow :: forall d. Event d -> Flow d
flow = identity


never :: forall d. Effect (Flow d)
never =
    create >>=
        \{ event } -> pure event


catMaybes
    :: forall f a
     . Foldable f
    => Monoid (f a)
    => Applicative f
    => f (Maybe a)
    -> f a
catMaybes seq =
    foldr eliminateMaybe (mempty :: f a) seq
    where
        eliminateMaybe (Just val) seq' = pure val <> seq'
        eliminateMaybe Nothing seq' = seq'
