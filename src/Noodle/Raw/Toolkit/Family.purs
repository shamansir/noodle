module Noodle.Raw.Toolkit.Family where

import Prelude

import Data.Map (Map)

import Effect.Class (class MonadEffect)

import Noodle.Id (FamilyR, InletR, OutletR, family) as Id
import Noodle.Repr (class FromRepr, class ToRepr)
import Noodle.Repr (ensureTo, unwrap) as Repr
import Noodle.Raw.Node (InletsValues, OutletsValues)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (_makeWithFn) as RawNode
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw


data Family state (repr :: Type) (m :: Type -> Type)
    = Family
        Id.FamilyR
        Raw.Shape
        state
        (InletsValues repr)
        (OutletsValues repr)
        (Raw.Fn state repr m)


id :: forall state repr m. Family state repr m -> Id.FamilyR
id (Family familyR _ _ _ _ _) = familyR


make
    :: forall state repr m
     . Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Process state repr m
    -> Family state repr m
make familyR state rawShape inletsMap outletsMap process = do
    Family
        familyR
        rawShape
        state
        inletsMap
        outletsMap
        $ RawFn.make (Id.family familyR) process


spawn ::
    forall m state repr mp
     . MonadEffect m
    => Family state repr mp
    -> m (Raw.Node state repr mp)
spawn (Family familyR rawShape state inletsMap outletsMap fn) =
    RawNode._makeWithFn
        familyR
        state
        rawShape
        inletsMap
        outletsMap
        fn


familyIdOf
    :: forall state repr m
     . Family state repr m
    -> Id.FamilyR
familyIdOf (Family rawId _ _ _ _ _) = rawId


toReprableState
    :: forall state repr m
     . FromRepr repr state
    => ToRepr state repr
    => Family state repr m
    -> Family repr repr m
toReprableState (Family familyR rawShape state inletsMap outletsMap fn) =
    Family
        familyR
        rawShape
        (Repr.unwrap $ Repr.ensureTo state)
        inletsMap
        outletsMap
        $ RawFn.toReprableState fn
