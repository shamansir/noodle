module Noodle.Node.Shaped
    where


import Prelude (pure, ($), (#), (>>>), (<$>), map)


import Noodle.Node as N
import Noodle.Node (Receive, Pass)
import Noodle.Shape as N

import Data.Array (snoc)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map.Extra (type (/->))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (lmap, rmap)

import Effect (Effect)


{-
class IsShaped m where
    inlet :: forall d. String -> m -> Maybe (Inlet d)
    outlet :: forall d. String -> m -> Maybe (Outlet d)
    inlets :: forall d. m -> Array (String /\ Inlet d)
    outlets :: forall d. m -> Array (String /\ Outlet d)

ShapeOf.inlet
ShapeOf.oulet
ShapeOf.inlets
ShapeOf.oulets
etc.
-}


newtype InletsShape d = InletsShape (Array (String /\ N.Shape d))
newtype OutletsShape d = OutletsShape (Array (String /\ N.Shape d))


type Node d =
    N.Node
        d
        ((String /-> N.Shape d) /\ (String /-> N.Shape d))


make
    :: forall d
    .  d
    -> InletsShape d
    -> OutletsShape d
    -> (Receive d -> Pass d)
    -> Effect (Node d)
make def (InletsShape inlets) (OutletsShape outlets) fn =
    let
        inletsMap = Map.fromFoldable inlets
        outletsMap = Map.fromFoldable outlets
    in N.make
        (inletsMap /\ outletsMap)
        def
        (\receive ->
            if inletsMap
                # Map.lookup (N.lastUpdateAt receive)
                # map N.isHot
                # fromMaybe true
            then fn receive
            else N.passNothing
        )


makeEff
    :: forall d
    .  d
    -> InletsShape d
    -> OutletsShape d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d)
makeEff def (InletsShape inlets) (OutletsShape outlets) fn =
    let
        inletsMap = Map.fromFoldable inlets
        outletsMap = Map.fromFoldable outlets
    in N.makeEff
        (inletsMap /\ outletsMap)
        def
        (\receive ->
            if inletsMap
                # Map.lookup (N.lastUpdateAt receive)
                # map N.isHot
                # fromMaybe true
            then fn receive
            else pure $ N.passNothing
        )


addInlet :: forall d. String -> N.Shape d -> Node d -> Node d
addInlet name shape = (<$>) (lmap $ Map.insert name shape)


reshapeInlet :: forall d. String -> N.Shape d -> Node d -> Node d
reshapeInlet = addInlet


addOutlet :: forall d. String -> N.Shape d -> Node d -> Node d
addOutlet name shape = (<$>) (rmap $ Map.insert name shape)


reshapeOutlet :: forall d. String -> N.Shape d -> Node d -> Node d
reshapeOutlet = addOutlet


inletShape :: forall d. String -> Node d -> Maybe (N.Shape d)
inletShape inlet = N.get >>> Tuple.snd >>> Map.lookup inlet


outletShape :: forall d. String -> Node d -> Maybe (N.Shape d)
outletShape outlet = N.get >>> Tuple.fst >>> Map.lookup outlet


withInlets :: forall d. Array (String /\ N.Shape d) -> InletsShape d
withInlets = InletsShape


withOutlets :: forall d. Array (String /\ N.Shape d) -> OutletsShape d
withOutlets = OutletsShape


infixl 1 andInlet as ~<

infixl 1 andOutlet as >~


andInlet
    :: forall d
     . InletsShape d
    -> String /\ N.Shape d
    -> InletsShape d
andInlet (InletsShape inlets) (name /\ shape) =
    InletsShape $ inlets `snoc` (name /\ shape)


andOutlet
    :: forall d
     . OutletsShape d
    -> String /\ N.Shape d
    -> OutletsShape d
andOutlet (OutletsShape outlets) (name /\ shape) =
    OutletsShape $ outlets `snoc` (name /\ shape)


noInlets :: forall d. InletsShape d
noInlets = InletsShape []


noOutlets :: forall d. OutletsShape d
noOutlets = OutletsShape []