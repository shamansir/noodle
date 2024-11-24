module Noodle.Raw.Toolkit.Family where

import Prelude

import Data.Map (Map)

import Effect.Class (class MonadEffect)

import Noodle.Id (FamilyR, InletR, OutletR, family) as Id
import Noodle.Raw.Node (InletsValues, OutletsValues)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (_makeWithFn) as RawNode
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw


data Family (repr :: Type) (m :: Type -> Type)
    = Family
        Id.FamilyR
        Raw.Shape
        repr
        (InletsValues repr)
        (OutletsValues repr)
        (Raw.Fn repr repr m)


make
    :: forall repr m
     . Id.FamilyR
    -> repr
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Process repr repr m
    -> Family repr m
make family state rawShape inletsMap outletsMap process = do
    Family
        family
        rawShape
        state
        inletsMap
        outletsMap
        $ RawFn.make (Id.family family) process


spawn ::
    forall repr mp m
     . MonadEffect m
    => Family repr mp
    -> m (Raw.Node repr mp)
spawn (Family familyR rawShape state inletsMap outletsMap fn) =
    RawNode._makeWithFn
        familyR
        state
        rawShape
        inletsMap
        outletsMap
        fn


familyIdOf
    :: forall repr m
     . Family repr m
    -> Id.FamilyR
familyIdOf (Family rawId _ _ _ _ _) = rawId
