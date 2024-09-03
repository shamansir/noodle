module Noodle.Toolkit.Families where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Repr (class ToReprRow)

import Type.Proxy (Proxy(..))

import Noodle.Id (Family(..), InletR, OutletR, FamilyR, family, familyR, inletR, outletR) as Id
import Noodle.Fn (Fn, RawFn)
import Noodle.Fn (make, makeRaw, run, run', toRaw) as Fn
import Noodle.Fn.Process (Process)
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (Raw, reflect, inletRName, outletRName) as Shape
import Noodle.Fn.Raw.Process (RawProcess)
import Noodle.Fn.RawToRec as ReprCnv

import Noodle.Node (Node, RawNode)
import Noodle.Node (make_, makeWithFn_, makeRawWithFn) as Node


infixr 6 type FCons as //


data Families
foreign import data FCons :: FamilyDef -> Families -> Families
foreign import data FNil :: Families


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef


class FamilyExistsIn (family :: FamilyDef) (families :: Families)
instance FamilyExistsIn family (FCons family tail)
else instance (FamilyExistsIn family tail) => FamilyExistsIn family (FCons skipfamily tail)



class PutFamily (family :: FamilyDef) (families :: Families) (families' :: Families)


instance PutFamily family FNil (FCons family FNil)
else instance PutFamily family (FCons some tail) (FCons family (FCons some tail))


data Family (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Family
        Shape.Raw
        state
        (Map Id.InletR repr)
        (Map Id.OutletR repr)
        (Fn state is os repr m)


data RawFamily (state :: Type) (repr :: Type) (m :: Type -> Type)
    = RawFamily
        Id.FamilyR
        Shape.Raw
        state
        (Map Id.InletR repr)
        (Map Id.OutletR repr)
        (RawFn state repr m)


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


makeRaw
    :: forall state repr m
     . Id.FamilyR
    -> state
    -> Shape.Raw
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> RawProcess state repr m
    -> RawFamily state repr m
makeRaw family state rawShape inletsMap outletsMap process = do
    RawFamily
        family
        rawShape
        state
        inletsMap
        outletsMap
        $ Fn.makeRaw (Id.family family) process


familyIdOf
    :: forall f state is os repr m
     . IsSymbol f
    => Family f state is os repr m
    -> Id.Family f
familyIdOf _ = Id.Family :: _ f



familyRIdOf
    :: forall state repr m
     . RawFamily state repr m
    -> Id.FamilyR
familyRIdOf (RawFamily rawId _ _ _ _ _) = rawId


spawn ::
    forall f state is os repr m
     . IsSymbol f
    => MonadEffect m
    => Family f state is os repr m
    -> m (Node f state is os repr m)
spawn family@(Family rawShape state inletsMap outletsMap fn) =
    Node.makeWithFn_
        (Id.familyR $ familyIdOf family)
        state
        rawShape
        inletsMap
        outletsMap
        fn


spawnRaw ::
    forall state repr m
     . MonadEffect m
    => RawFamily state repr m
    -> m (RawNode state repr m)
spawnRaw (RawFamily familyR rawShape state inletsMap outletsMap fn) =
    Node.makeRawWithFn
        familyR
        state
        rawShape
        inletsMap
        outletsMap
        fn