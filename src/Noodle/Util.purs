module Noodle.Util
    ( convertKeysInMap
    , convertMap
    , type (/->)
    , Subscriber, Canceler
    , Flow, PushF, PushableFlow(..)
    , flow, never
    , seqMember, seqMember', seqDelete, seqCatMaybes, (:), (+>), seqNub, seqNubBy
    , Position, Rect, Bounds, quickBounds, quickBounds'
    ) where


import Prelude

import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.List (nubBy) as List
import Data.Bifunctor (lmap, bimap)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Tuple.Nested ((/\), type (/\))

import FRP.Event (Event, create)


infixr 6 type Map as /->


type Position = { x :: Number, y :: Number }
type Rect = { width :: Number, height :: Number }
type Bounds = Position /\ Rect


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


-- TODO: place in Data.Seq
seqMember :: forall a. Eq a => a -> Seq a -> Boolean
seqMember v seq =
    Seq.length (Seq.filter ((==) v) seq) > 0


seqMember' :: forall a. Eq a => a -> Seq a -> Maybe Unit
seqMember' v seq =
    if seqMember v seq then Just unit else Nothing


seqDelete :: forall a. Eq a => a -> Seq a -> Seq a
seqDelete v seq =
    Seq.filter ((/=) v) seq


seqCatMaybes :: forall a. Seq (Maybe a) -> Seq a
seqCatMaybes seq =
    foldr eliminateMaybe Seq.empty seq
    where
        eliminateMaybe (Just val) seq' = Seq.cons val seq'
        eliminateMaybe Nothing seq' = seq'


seqNub :: forall a. Eq a => Seq a -> Seq a
seqNub = seqNubBy (==)


seqNubBy :: forall a. (a -> a -> Boolean) -> Seq a -> Seq a
seqNubBy eq =
    Seq.toUnfoldable >>> List.nubBy eq >>> Seq.fromFoldable


-- TODO: place in Data.Seq
infixr 6 Seq.cons as :
infixl 6 Seq.snoc as +>
