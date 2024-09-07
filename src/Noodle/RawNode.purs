module Noodle.RawNode where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.UniqueHash (generate) as UH
import Data.Repr (class ToRepr, class FromRepr)
import Data.Tuple.Nested ((/\))

import Noodle.Id (NodeR, FamilyR, InletR, OutletR, family, nodeRaw) as Id
import Noodle.Node (Node(..))
import Noodle.Fn (RawFn)
import Noodle.Fn (makeRaw, toRawWithReprableState) as Fn
import Noodle.Fn.Raw.Process (RawProcess)
import Noodle.Fn.Shape (Raw) as Shape
import Noodle.Fn.Protocol (make) as Protocol
import Noodle.Fn.Raw.Tracker (Tracker) as Raw
import Noodle.Fn.Raw.Protocol (Protocol) as Raw
import Noodle.Fn.Raw.Protocol (toReprableState) as RawProtocol
import Noodle.Fn.Raw.Tracker (toReprableState) as RawTracker


data RawNode (repr :: Type) (m :: Type -> Type)
    = RawNode
        Id.NodeR
        Shape.Raw
        (Raw.Tracker repr repr)
        (Raw.Protocol repr repr)
        (RawFn repr repr m)


makeRaw
    :: forall repr m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Shape.Raw
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> RawProcess repr repr m
    -> m (RawNode repr m)
makeRaw family state rawShape inletsMap outletsMap process = do
    makeRawWithFn family state rawShape inletsMap outletsMap $ Fn.makeRaw (Id.family family) process


makeRawWithFn
    :: forall repr m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Shape.Raw
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> RawFn repr repr m
    -> m (RawNode repr m)
makeRawWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeRaw family uniqueHash
    tracker /\ protocol <- Protocol.make state inletsMap outletsMap
    pure $ RawNode nodeId rawShape tracker protocol fn


{- Rawify -}


toRaw :: forall f state is os repr m. FromRepr repr state => ToRepr state repr => Node f state is os repr m -> RawNode repr m
toRaw (Node nodeR shape tracker protocol fn) =
    RawNode nodeR shape
        (RawTracker.toReprableState tracker)
        (RawProtocol.toReprableState protocol)
        $ Fn.toRawWithReprableState fn