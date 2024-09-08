module Noodle.Raw.Node where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.UniqueHash (generate) as UH
import Data.Tuple.Nested ((/\))

import Noodle.Id (NodeR, FamilyR, InletR, OutletR, family, nodeRaw) as Id
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Fn.Protocol (make) as Protocol
import Noodle.Raw.Fn.Tracker (Tracker) as Raw
import Noodle.Raw.Fn.Protocol (Protocol) as Raw


data Node (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Raw.Tracker repr repr)
        (Raw.Protocol repr repr)
        (Raw.Fn repr repr m)


make
    :: forall repr m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Raw.Shape
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> Raw.Process repr repr m
    -> m (Node repr m)
make family state rawShape inletsMap outletsMap process = do
    makeWithFn family state rawShape inletsMap outletsMap $ RawFn.make (Id.family family) process


makeWithFn
    :: forall repr m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Raw.Shape
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> Raw.Fn repr repr m
    -> m (Node repr m)
makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeRaw family uniqueHash
    tracker /\ protocol <- Protocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn