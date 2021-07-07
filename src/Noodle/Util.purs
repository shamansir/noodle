module Noodle.Util
    ( convertKeysInMap
    , convertMap
    , type (/->)
    , Subscriber, Canceler
    , Flow, PushF, PushableFlow(..)
    , flow, never
    , Position, Rect, Bounds, quickBounds, quickBounds'
    , merge
    ) where


import Prelude

import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (class Foldable, foldr)
import Data.Bifunctor (lmap, bimap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', Prism', lens, view, preview, set, review)

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


-- FIXME: could be illegal, by law has to be `Traversal`
merge
    :: forall s a b
     . Lens' s (Maybe a)
    -> Prism' a b
    -> Lens' s (Maybe b)
merge l p =
    lens getter setter
    where
        getter s = view l s >>= preview p
        setter s maybeX = set l (review p <$> maybeX) s


infixr 6 merge as <|<


-- FIXME: could be illegal, by law has to be `Traversal`
{-
lfish
    :: forall s a b
     . Lens' s (Maybe a)
    -> Prism' a b
    -> Lens' s (Maybe b)
lfish fst snd =
    lens getter setter
    where
        getter s = preview fst s <#> ?wh
        setter s (Just b) = set fst (Just $ review snd b) s
        setter s Nothing = set fst Nothing s
-}
