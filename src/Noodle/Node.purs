module Noodle.Node where

import Prelude

import Prim.RowList as RL

import Effect.Class (class MonadEffect, liftEffect)

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.UniqueHash (generate) as UH
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class ToReprRow)

import Noodle.Id as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make) as Fn
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (Raw, reflect) as Shape
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol (make) as Protocol
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.Process (ProcessM)
import Noodle.Fn.RawToRec as ReprCnv


data Node (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Shape.Raw
        (Tracker state is os repr)
        (Protocol state is os repr)
        (Fn state is os repr m)


make
    :: forall f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) repr m
     . IsSymbol f
    => InletsDefs inlets => OutletsDefs outlets
    => ToReprRow isrl is Id.InletR repr => ToReprRow osrl os Id.OutletR repr
    -- => ContainsAllInlets isrl inlets => ContainsAllOutlets osrl outlets
    => MonadEffect m
    => Id.Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> ProcessM state is os repr m Unit
    -> m (Node f state is os repr m)
make family state shape inletsRec outletsRec process =
    makeRaw
        (Id.familyR family)
        state
        (Shape.reflect shape)
        (ReprCnv.fromRec Id.inletR inletsRec)
        (ReprCnv.fromRec Id.outletR outletsRec)
        process


makeRaw
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Shape.Raw
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> ProcessM state is os repr m Unit
    -> m (Node f state is os repr m)
makeRaw family state rawShape inletsMap outletsMap process = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeRaw family uniqueHash
    tracker /\ protocol <- Protocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol $ Fn.make (Id.family family) process