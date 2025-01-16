module Noodle.Toolkit.Family where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Symbol (class IsSymbol, reflectSymbol)

import Type.Proxy (Proxy(..))

import Noodle.Id (Family(..), InletR, OutletR, familyR, inletR, outletR) as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make, toRaw) as Fn
import Noodle.Fn.Process (Process)
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Node (Node)
import Noodle.Node (_makeWithFn) as Node
import Noodle.Repr.ValueInChannel (class FromValuesInChannelRow)
import Noodle.Repr.ValueInChannel (toFallback) as ViC
import Noodle.Repr.HasFallback (class HasFallback)

import Noodle.Raw.Node (InitialInletsValues, InitialOutletsValues) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.FromToRec as ChReprCnv
import Noodle.Raw.Toolkit.Family (Family(..)) as Raw


data Family (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (chrepr :: Type) (m :: Type -> Type)
    = Family
        Raw.Shape
        state
        (Raw.InitialInletsValues chrepr)
        (Raw.InitialOutletsValues chrepr)
        (Fn state is os chrepr m)


make
    :: forall f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) chrepr m
     . IsSymbol f
    => HasFallback chrepr
    => InletsDefs inlets => OutletsDefs outlets
    => FromValuesInChannelRow isrl is Id.InletR chrepr
    => FromValuesInChannelRow osrl os Id.OutletR chrepr
    => ContainsAllInlets is inlets => ContainsAllOutlets os outlets
    => Id.Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> Process state is os chrepr m
    -> Family f state is os chrepr m
make _ state shape inletsRec outletsRec process =
    Family
        (Shape.reflect shape)
        state
        (ViC.toFallback <$> ChReprCnv.fromRec Id.inletR inletsRec)
        (ViC.toFallback <$> ChReprCnv.fromRec Id.outletR outletsRec)
        $ Fn.make (reflectSymbol (Proxy :: _ f)) process


familyIdOf
    :: forall f state is os chrepr m
     . IsSymbol f
    => Family f state is os chrepr m
    -> Id.Family f
familyIdOf _ = Id.Family :: _ f



spawn ::
    forall f state is os chrepr mp m
     . IsSymbol f
    => MonadEffect m
    => Family f state is os chrepr mp
    -> m (Node f state is os chrepr mp)
spawn family@(Family rawShape state inletsMap outletsMap fn) =
    Node._makeWithFn
        (Id.familyR $ familyIdOf family)
        state
        rawShape
        inletsMap
        outletsMap
        fn


toRaw :: forall f state is os chrepr m
     . IsSymbol f
    => Family f state is os chrepr m
    -> Raw.Family state chrepr m
toRaw family@(Family rawShape state inletsMap outletsMap fn) =
    Raw.Family
        (Id.familyR $ familyIdOf family)
        rawShape
        state
        inletsMap
        outletsMap
        $ Fn.toRaw fn
