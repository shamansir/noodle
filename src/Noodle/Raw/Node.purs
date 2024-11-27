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
import Noodle.Fn.Generic.Updates (UpdateFocus(..)) as Fn
import Noodle.Fn.Generic.Updates (toTuple) as Updates
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, run', toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Protocol (make, getInlets, getOutlets, getState, sendIn) as RawProtocol
import Noodle.Raw.Fn.Tracker (Tracker) as Raw
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Raw.Fn.Tracker (toReprableState) as RawTracker
import Noodle.Raw.Fn.Protocol (toReprableState) as RawProtocol
import Noodle.Repr (class HasFallback, class ToRepr, class FromRepr)


type InletsValues  repr = Map Id.InletR repr
type OutletsValues repr = Map Id.OutletR repr


type OrderedInletsValues  repr = Map (Int /\ Id.InletR) repr
type OrderedOutletsValues repr = Map (Int /\ Id.OutletR) repr


data Node state (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Raw.Tracker state repr)
        (Raw.Protocol state repr)
        (Raw.Fn state repr m)


{- Get info -}


family :: forall state repr m. Node state repr m -> Id.FamilyR
family = id >>> Id.familyOf


id :: forall state repr m. Node state repr m -> Id.NodeR
id (Node nodeR _ _ _ _) = nodeR


{- Making -}


make
    :: forall m state repr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Process state repr mp
    -> m (Node state repr mp)
make family state rawShape inletsMap outletsMap process = do
    _makeWithFn family state rawShape inletsMap outletsMap $ RawFn.make (Id.family family) process


_makeWithFn
    :: forall m state repr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues repr
    -> OutletsValues repr
    -> Raw.Fn state repr mp
    -> m (Node state repr mp)
_makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeR_ family uniqueHash
    tracker /\ protocol <- RawProtocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn


{- Running -}


-- TODO: private
_runOnInletUpdates
    :: forall state repr m
    .  Wiring m
    => HasFallback repr
    => Node state repr m
    -> m Unit
_runOnInletUpdates node =
  SignalX.runSignal $ subscribeInlets node ~> const (run node)


-- TODO: private
_runOnStateUpdates
    :: forall state repr m
    .  Wiring m
    => HasFallback repr
    => Node state repr m
    -> m Unit
_runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
_listenUpdatesAndRun
  :: forall state repr m
   . Wiring m
  => HasFallback repr
  => Node state repr m
  -> m Unit
_listenUpdatesAndRun node = do
  _runOnInletUpdates node
  -- this leading us into a loop when `modifyState` is inside the `Node`' process call
  --runOnStateUpdates node -- may be running on state updates is not needed;
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall state repr m. MonadRec m => MonadEffect m => HasFallback repr => Node state repr m -> m Unit
run (Node _ _ _ protocol fn) = RawFn.run' protocol fn


{- Get Data -}


shape :: forall state repr m. Node state repr m -> Raw.Shape
shape (Node _ shape _ _ _) = shape


inlets :: forall m state repr mp. MonadEffect m => Node state repr mp -> m (InletsValues repr)
inlets node = liftEffect $ RawProtocol.getInlets $ _getProtocol node


outlets :: forall m state repr mp. MonadEffect m => Node state repr mp -> m (OutletsValues repr)
outlets node = liftEffect $ RawProtocol.getOutlets $ _getProtocol node


state :: forall m state repr mp. MonadEffect m => Node state repr mp -> m state
state node = liftEffect $ RawProtocol.getState $ _getProtocol node


atInlet :: forall m state repr mp. MonadEffect m => Id.InletR -> Node state repr mp -> m (Maybe repr)
atInlet inlet node = inlets node <#> Map.lookup inlet


atOutlet :: forall m state repr mp. MonadEffect m => Id.OutletR -> Node state repr mp -> m (Maybe repr)
atOutlet outlet node = outlets node <#> Map.lookup outlet


curChanges :: forall m state repr mp. MonadEffect m => Node state repr mp -> m (NodeChanges state repr)
curChanges node = do
  is <- inlets node
  os <- outlets node
  s  <- state node
  pure $ Fn.Everything /\ s /\ is /\ os


{- Private accessors -}


_getProtocol :: forall state repr m. Node state repr m -> Raw.Protocol state repr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall state repr m. Node state repr m -> Raw.Tracker state repr
_getTracker (Node _ _ tracker _ _) = tracker


{- Subscriptions -}


type NodeChanges state repr = (Fn.UpdateFocus /\ state /\ InletsValues repr /\ OutletsValues repr)


subscribeInlet :: forall state repr m. Id.InletR -> Node state repr m -> Signal (Maybe repr)
subscribeInlet input node = Map.lookup input <$> subscribeInlets node


subscribeInlets :: forall state repr m. Node state repr m -> Signal (InletsValues repr)
subscribeInlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


subscribeOutlet :: forall state repr m. Id.OutletR -> Node state repr m -> Signal (Maybe repr)
subscribeOutlet output node = Map.lookup output <$> subscribeOutlets node


subscribeOutlets :: forall state repr m. Node state repr m -> Signal (OutletsValues repr)
subscribeOutlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeState :: forall state repr m. Node state repr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall state repr m. Node state repr m -> Signal (NodeChanges state repr)
subscribeChanges (Node _ _ tracker _ _) = tracker.all <#> Updates.toTuple


{- Send data -}


sendIn :: forall m state repr mp. MonadEffect m => Id.InletR -> repr -> Node state repr mp -> m Unit
sendIn input din = liftEffect <<< RawProtocol.sendIn input din <<< _getProtocol


-- TODO:


{- Convert -}


toReprableState :: forall state repr m. FromRepr repr state => ToRepr state repr => Node state repr m -> Node repr repr m
toReprableState (Node nodeR shape tracker protocol fn) =
    Node
        nodeR
        shape
        (RawTracker.toReprableState tracker)
        (RawProtocol.toReprableState protocol)
        $ RawFn.toReprableState fn