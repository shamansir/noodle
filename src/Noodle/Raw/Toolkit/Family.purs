module Noodle.Raw.Toolkit.Family where

import Prelude

import Data.Map (Map)

import Effect.Class (class MonadEffect)

import Noodle.Id (FamilyR, InletR, OutletR, family) as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (to) as StRepr
import Noodle.Raw.Node (InletsValues, OutletsValues)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (_makeWithFn) as RawNode
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw


data Family state (chrepr :: Type) (m :: Type -> Type)
    = Family
        Id.FamilyR
        Raw.Shape
        state
        (InletsValues chrepr)
        (OutletsValues chrepr)
        (Raw.Fn state chrepr m)


id :: forall state chrepr m. Family state chrepr m -> Id.FamilyR
id (Family familyR _ _ _ _ _) = familyR


make
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues chrepr
    -> OutletsValues chrepr
    -> Raw.Process state chrepr m
    -> Family state chrepr m
make familyR state rawShape inletsMap outletsMap process = do
    Family
        familyR
        rawShape
        state
        inletsMap
        outletsMap
        $ RawFn.make (Id.family familyR) process


spawn ::
    forall m state chrepr mp
     . MonadEffect m
    => Family state chrepr mp
    -> m (Raw.Node state chrepr mp)
spawn (Family familyR rawShape state inletsMap outletsMap fn) =
    RawNode._makeWithFn
        familyR
        state
        rawShape
        inletsMap
        outletsMap
        fn


familyIdOf
    :: forall state chrepr m
     . Family state chrepr m
    -> Id.FamilyR
familyIdOf (Family rawId _ _ _ _ _) = rawId


toReprableState
    :: forall state strepr chrepr m
     . HasFallback state
    => StRepr state strepr
    => Family state chrepr m
    -> Family strepr chrepr m
toReprableState (Family familyR rawShape state inletsMap outletsMap fn) =
    Family
        familyR
        rawShape
        (StRepr.to state)
        inletsMap
        outletsMap
        $ RawFn.toReprableState fn
