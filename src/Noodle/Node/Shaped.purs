module Noodle.Node.Shaped
    where


import Prelude (pure, ($), (#), (>>>), (<$>), map)


import Noodle.Node as N
import Noodle.Node (Receive, Pass)
import Noodle.Channel.Shape as Channel
import Noodle.Node.Shape (Shape)

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


newtype InletsShape d = InletsShape (Array (String /\ Channel.Shape d))
newtype OutletsShape d = OutletsShape (Array (String /\ Channel.Shape d))


type Node d =
    N.Node d (Shape d)


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
                # map Channel.isHot
                # fromMaybe true
            then fn receive
            else N.passNothing
        )


makeEffectful
    :: forall d
    .  d
    -> InletsShape d
    -> OutletsShape d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d)
makeEffectful def (InletsShape inlets) (OutletsShape outlets) fn =
    let
        inletsMap = Map.fromFoldable inlets
        outletsMap = Map.fromFoldable outlets
    in N.makeEffectful
        (inletsMap /\ outletsMap)
        def
        (\receive ->
            if inletsMap
                # Map.lookup (N.lastUpdateAt receive)
                # map Channel.isHot
                # fromMaybe true
            then fn receive
            else pure $ N.passNothing
        )


addInlet :: forall d. String -> Channel.Shape d -> Node d -> Node d
addInlet name shape = (<$>) (lmap $ Map.insert name shape)


reshape :: forall d. (InletsShape d /\ OutletsShape d) -> Node d -> Node d
reshape (InletsShape inlets /\ OutletsShape outlets) =
    N.set
        (Map.fromFoldable inlets /\ Map.fromFoldable outlets)
        -- FIXME: update the handler to monitor hot/cold inlets as well


reshapeInlet :: forall d. String -> Channel.Shape d -> Node d -> Node d
reshapeInlet = addInlet


addOutlet :: forall d. String -> Channel.Shape d -> Node d -> Node d
addOutlet name shape = (<$>) (rmap $ Map.insert name shape)


reshapeOutlet :: forall d. String -> Channel.Shape d -> Node d -> Node d
reshapeOutlet = addOutlet


inletShape :: forall d. String -> Node d -> Maybe (Channel.Shape d)
inletShape inlet = N.get >>> Tuple.snd >>> Map.lookup inlet


outletShape :: forall d. String -> Node d -> Maybe (Channel.Shape d)
outletShape outlet = N.get >>> Tuple.fst >>> Map.lookup outlet


withInlets :: forall d. Array (String /\ Channel.Shape d) -> InletsShape d
withInlets = InletsShape


withOutlets :: forall d. Array (String /\ Channel.Shape d) -> OutletsShape d
withOutlets = OutletsShape


infixl 1 andInlet as ~<

infixl 1 andOutlet as >~


andInlet
    :: forall d
     . InletsShape d
    -> String /\ Channel.Shape d
    -> InletsShape d
andInlet (InletsShape inlets) (name /\ shape) =
    InletsShape $ inlets `snoc` (name /\ shape)


andOutlet
    :: forall d
     . OutletsShape d
    -> String /\ Channel.Shape d
    -> OutletsShape d
andOutlet (OutletsShape outlets) (name /\ shape) =
    OutletsShape $ outlets `snoc` (name /\ shape)


noInlets :: forall d. InletsShape d
noInlets = InletsShape []


noOutlets :: forall d. OutletsShape d
noOutlets = OutletsShape []