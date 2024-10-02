module Noodle.Toolkit.Family where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Noodle.Repr (class ToReprRow)

import Type.Proxy (Proxy(..))

import Noodle.Id (Family(..), InletR, OutletR, familyR, inletR, outletR) as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make) as Fn
import Noodle.Fn.Process (Process)
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Node (Node)
import Noodle.Node (_makeWithFn) as Node

import Noodle.Raw.Node (InletsValues, OutletsValues) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.FromToRec as ReprCnv


data Family (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Family
        Raw.Shape
        state
        (Raw.InletsValues repr)
        (Raw.OutletsValues repr)
        (Fn state is os repr m)


make
    :: forall f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) repr m
     . IsSymbol f
    => InletsDefs inlets => OutletsDefs outlets
    => ToReprRow isrl is Id.InletR repr => ToReprRow osrl os Id.OutletR repr
    => ContainsAllInlets is inlets => ContainsAllOutlets os outlets
    => Id.Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> Process state is os repr m
    -> Family f state is os repr m
make _ state shape inletsRec outletsRec process =
    Family
        (Shape.reflect shape)
        state
        (ReprCnv.fromRec Id.inletR inletsRec)
        (ReprCnv.fromRec Id.outletR outletsRec)
        $ Fn.make (reflectSymbol (Proxy :: _ f)) process


familyIdOf
    :: forall f state is os repr m
     . IsSymbol f
    => Family f state is os repr m
    -> Id.Family f
familyIdOf _ = Id.Family :: _ f



spawn ::
    forall f state is os repr m
     . IsSymbol f
    => MonadEffect m
    => Family f state is os repr m
    -> m (Node f state is os repr m)
spawn family@(Family rawShape state inletsMap outletsMap fn) =
    Node._makeWithFn
        (Id.familyR $ familyIdOf family)
        state
        rawShape
        inletsMap
        outletsMap
        fn