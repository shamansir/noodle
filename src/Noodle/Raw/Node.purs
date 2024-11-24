module Noodle.Raw.Node where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.Rec.Class (class MonadRec)

import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.UniqueHash (generate) as UH
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Signal (Signal, (~>))
import Signal.Extra (runSignal) as SignalX

import Noodle.Wiring (class Wiring)
import Noodle.Id (NodeR, FamilyR, InletR, OutletR, family, familyOf, nodeR_) as Id
import Noodle.Fn.Generic.Updates (UpdateFocus) as Fn
import Noodle.Fn.Generic.Updates (toTuple) as Updates
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, run') as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Protocol (make, getInlets, getOutlets, getState) as RawProtocol
import Noodle.Raw.Fn.Tracker (Tracker) as Raw
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Repr (class HasFallback)


type InletsValues repr = Map Id.InletR repr
type OutletsValues repr = Map Id.OutletR repr


data Node (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Raw.Tracker repr repr)
        (Raw.Protocol repr repr)
        (Raw.Fn repr repr m)


{- Get info -}


family :: forall repr m. Node repr m -> Id.FamilyR
family = id >>> Id.familyOf


id :: forall repr m. Node repr m -> Id.NodeR
id (Node nodeR _ _ _ _) = nodeR


{- Making -}


make
    :: forall repr mp m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Process repr repr mp
    -> m (Node repr mp)
make family state rawShape inletsMap outletsMap process = do
    _makeWithFn family state rawShape inletsMap outletsMap $ RawFn.make (Id.family family) process


_makeWithFn
    :: forall repr mp m
     . MonadEffect m
    => Id.FamilyR
    -> repr
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Fn repr repr mp
    -> m (Node repr mp)
_makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeR_ family uniqueHash
    tracker /\ protocol <- RawProtocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn


{- Running -}


-- TODO: private
_runOnInletUpdates
    :: forall repr m
    .  Wiring m
    => HasFallback repr
    => Node repr m
    -> m Unit
_runOnInletUpdates node =
  SignalX.runSignal $ subscribeInlets node ~> const (run node)


-- TODO: private
_runOnStateUpdates
    :: forall repr m
    .  Wiring m
    => HasFallback repr
    => Node repr m
    -> m Unit
_runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
_listenUpdatesAndRun
  :: forall repr m
   . Wiring m
    => HasFallback repr
  => Node repr m
  -> m Unit
_listenUpdatesAndRun node = do
  _runOnInletUpdates node
  -- this leading us into a loop when `modifyState` is inside the `Node`' process call
  --runOnStateUpdates node -- may be running on state updates is not needed;
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall repr m. MonadRec m => MonadEffect m => HasFallback repr => Node repr m -> m Unit
run (Node _ _ _ protocol fn) = RawFn.run' protocol fn


{- Get Data -}


inlets :: forall repr m. MonadEffect m => Node repr m -> m (InletsValues repr)
inlets node = liftEffect $ RawProtocol.getInlets $ _getProtocol node


outlets :: forall repr m. MonadEffect m => Node repr m -> m (OutletsValues repr)
outlets node = liftEffect $ RawProtocol.getOutlets $ _getProtocol node


state :: forall repr m. MonadEffect m => Node repr m -> m repr
state node = liftEffect $ RawProtocol.getState $ _getProtocol node


atInlet :: forall repr m. MonadEffect m => Id.InletR -> Node repr m -> m (Maybe repr)
atInlet inlet node = inlets node <#> Map.lookup inlet


atOutlet :: forall repr m. MonadEffect m => Id.OutletR -> Node repr m -> m (Maybe repr)
atOutlet outlet node = outlets node <#> Map.lookup outlet


{- Private accessors -}


_getProtocol :: forall repr m. Node repr m -> Raw.Protocol repr repr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall repr m. Node repr m -> Raw.Tracker repr repr
_getTracker (Node _ _ tracker _ _) = tracker


{- Subscriptions -}


subscribeInlet :: forall repr m. Id.InletR -> Node repr m -> Signal (Maybe repr)
subscribeInlet input node = Map.lookup input <$> subscribeInlets node


subscribeInlets :: forall repr m. Node repr m -> Signal (InletsValues repr)
subscribeInlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


subscribeOutlet :: forall repr m. Id.OutletR -> Node repr m -> Signal (Maybe repr)
subscribeOutlet output node = Map.lookup output <$> subscribeOutlets node


subscribeOutlets :: forall repr m. Node repr m -> Signal (OutletsValues repr)
subscribeOutlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeState :: forall repr m. Node repr m -> Signal repr
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall repr m. Node repr m -> Signal (Fn.UpdateFocus /\ repr /\ InletsValues repr /\ OutletsValues repr)
subscribeChanges (Node _ _ tracker _ _) = tracker.all <#> Updates.toTuple
