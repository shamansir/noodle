module Noodle.Raw.Toolkit.Family where

import Prelude

import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Effect.Class (class MonadEffect)

import Noodle.Id (FamilyR, InletR, OutletR, family, unsafeInletR, unsafeOutletR) as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (to) as StRepr
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Raw.Node (InitialInletsValues, InitialOutletsValues)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (_makeWithFn) as RawNode
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (qmake_) as RawShape
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Fn.Shape.Temperament (Algorithm, defaultAlgorithm) as Temp


data Family state (chrepr :: Type) (m :: Type -> Type)
    = Family
        Id.FamilyR
        Raw.Shape
        state
        (InitialInletsValues chrepr)
        (InitialOutletsValues chrepr)
        (Raw.Fn state chrepr m)


id :: forall state chrepr m. Family state chrepr m -> Id.FamilyR
id (Family familyR _ _ _ _ _) = familyR


make
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> Raw.Shape
    -> InitialInletsValues chrepr
    -> InitialOutletsValues chrepr
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
    => VT.ValueTagged chrepr
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


qmake
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr m
    -> Family state chrepr m
qmake familyR state toRepr =
    qmake_ familyR state toRepr Temp.defaultAlgorithm


qmake_
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    -> Temp.Algorithm
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr m
    -> Family state chrepr m
qmake_ familyR state toRepr tempAlgo { inlets, outlets } process =
    make
        familyR
        state
        (RawShape.qmake_ tempAlgo
            { inlets : cutValue <$> inlets
            , outlets : cutValue <$> outlets }
        )
        ( Map.fromFoldable $ iConvert <$> inlets )
        ( Map.fromFoldable $ oConvert <$> outlets )
        process
    where
        cutValue { name, tag } = { name, tag }
        iConvert { name, tag, value } = Id.unsafeInletR  name /\ toRepr { name, tag, value }
        oConvert { name, tag, value } = Id.unsafeOutletR name /\ toRepr { name, tag, value }
