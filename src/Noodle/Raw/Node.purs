module Noodle.Raw.Node where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write) as Ref

import Control.Monad.Rec.Class (class MonadRec)

import Data.Map (Map)
import Data.Map (lookup, fromFoldable, toUnfoldable, mapMaybeWithKey) as Map
import Data.UniqueHash (generate) as UH
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Signal (Signal, (~>))
import Signal.Extra (runSignal) as SignalX

import Noodle.Wiring (class Wiring)
import Noodle.Id (NodeR, FamilyR, InletR, OutletR, family, familyOf, nodeR_) as Id
import Noodle.Fn.Generic.Updates (UpdateFocus(..)) as Fn
import Noodle.Fn.Generic.Updates (toRecord) as Updates
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ChRepr (class ToChRepr, class FromChRepr)
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, run', toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (inlets, outlets) as RawShape
import Noodle.Raw.Fn.Protocol (make, getInlets, getOutlets, getState, sendIn) as RawProtocol
import Noodle.Raw.Fn.Tracker (Tracker) as Raw
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Raw.Fn.Tracker (toReprableState) as RawTracker
import Noodle.Raw.Fn.Protocol (toReprableState) as RawProtocol
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (make, fromNode, toNode, cancel) as RawLink


data Node state (chrepr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Raw.Tracker state chrepr)
        (Raw.Protocol state chrepr)
        (Raw.Fn state chrepr m)


type InletsValues  chrepr = Map Id.InletR chrepr
type OutletsValues chrepr = Map Id.OutletR chrepr


type OrderedInletsValues  chrepr = Map (Int /\ Id.InletR)  chrepr
type OrderedOutletsValues chrepr = Map (Int /\ Id.OutletR) chrepr


{- Get info -}


family :: forall state chrepr m. Node state chrepr m -> Id.FamilyR
family = id >>> Id.familyOf


id :: forall state chrepr m. Node state chrepr m -> Id.NodeR
id (Node nodeR _ _ _ _) = nodeR


{- Making -}


make
    :: forall m state chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues  chrepr
    -> OutletsValues chrepr
    -> Raw.Process state chrepr mp
    -> m (Node state chrepr mp)
make family state rawShape inletsMap outletsMap process = do
    _makeWithFn family state rawShape inletsMap outletsMap $ RawFn.make (Id.family family) process


_makeWithFn
    :: forall m state chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> InletsValues  chrepr
    -> OutletsValues chrepr
    -> Raw.Fn state chrepr mp
    -> m (Node state chrepr mp)
_makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeR_ family uniqueHash
    tracker /\ protocol <- RawProtocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn


{- Running -}


-- TODO: private
_runOnInletUpdates
    :: forall state chrepr m
    .  Wiring m
    => HasFallback chrepr
    => Node state chrepr m
    -> m Unit
_runOnInletUpdates node =
  SignalX.runSignal $ subscribeInlets node ~> const (run node)


-- TODO: private
_runOnStateUpdates
    :: forall state chrepr m
    .  Wiring m
    => HasFallback chrepr
    => Node state chrepr m
    -> m Unit
_runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
_listenUpdatesAndRun
  :: forall state chrepr m
   . Wiring m
  => HasFallback chrepr
  => Node state chrepr m
  -> m Unit
_listenUpdatesAndRun node = do
  _runOnInletUpdates node
  -- this leading us into a loop when `modifyState` is inside the `Node`' process call
  --runOnStateUpdates node -- may be running on state updates is not needed;
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall state chrepr m. MonadRec m => MonadEffect m => HasFallback chrepr => Node state chrepr m -> m Unit
run (Node _ _ _ protocol fn) = RawFn.run' protocol fn


{- Get Data -}


shape :: forall state chrepr m. Node state chrepr m -> Raw.Shape
shape (Node _ shape _ _ _) = shape


inlets :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (InletsValues chrepr)
inlets node = liftEffect $ RawProtocol.getInlets $ _getProtocol node


outlets :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (OutletsValues chrepr)
outlets node = liftEffect $ RawProtocol.getOutlets $ _getProtocol node


state :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m state
state node = liftEffect $ RawProtocol.getState $ _getProtocol node


atInlet :: forall m state chrepr mp. MonadEffect m => Id.InletR -> Node state chrepr mp -> m (Maybe chrepr)
atInlet inlet node = inlets node <#> Map.lookup inlet


atOutlet :: forall m state chrepr mp. MonadEffect m => Id.OutletR -> Node state chrepr mp -> m (Maybe chrepr)
atOutlet outlet node = outlets node <#> Map.lookup outlet


curChanges :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (NodeChanges state chrepr)
curChanges node = do
  is <- inlets node
  os <- outlets node
  s  <- state node
  pure { focus : Fn.Everything, state : s, inlets : is, outlets : os }


{- Private accessors -}


_getProtocol :: forall state chrepr m. Node state chrepr m -> Raw.Protocol state chrepr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall state chrepr m. Node state chrepr m -> Raw.Tracker state chrepr
_getTracker (Node _ _ tracker _ _) = tracker


{- Subscriptions -}


type NodeChanges state chrepr = { focus :: Fn.UpdateFocus, state :: state, inlets :: InletsValues chrepr, outlets :: OutletsValues chrepr }


subscribeInlet :: forall state chrepr m. Id.InletR -> Node state chrepr m -> Signal (Maybe chrepr)
subscribeInlet input node = Map.lookup input <$> subscribeInlets node


subscribeInlets :: forall state chrepr m. Node state chrepr m -> Signal (InletsValues chrepr)
subscribeInlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


subscribeOutlet :: forall state chrepr m. Id.OutletR -> Node state chrepr m -> Signal (Maybe chrepr)
subscribeOutlet output node = Map.lookup output <$> subscribeOutlets node


subscribeOutlets :: forall state chrepr m. Node state chrepr m -> Signal (OutletsValues chrepr)
subscribeOutlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeState :: forall state chrepr m. Node state chrepr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall state chrepr m. Node state chrepr m -> Signal (NodeChanges state chrepr)
subscribeChanges (Node _ _ tracker _ _) = tracker.all <#> Updates.toRecord


{- Send data -}


sendIn :: forall m state chrepr mp. MonadEffect m => Id.InletR -> chrepr -> Node state chrepr mp -> m Unit
sendIn input din = liftEffect <<< RawProtocol.sendIn input din <<< _getProtocol


-- TODO:


{- Connecting -}


-- TODO: reuse those in `Noodle.Node` implementation instead of what is implemented there


connect
    :: forall m stateA stateB chreprA chreprB mp
     . Wiring m
    => HasFallback chreprA
    => HasFallback chreprB
    => Id.OutletR
    -> Id.InletR
    -> (chreprA -> chreprB)
    -> Node stateA chreprA mp
    -> Node stateB chreprB mp
    -> m Raw.Link
connect
    outletA
    inletB
    convertRepr
    nodeA@(Node nodeAId _ _ _ _)
    nodeB@(Node nodeBId _ _ _ _) =
    do
        flagRef <- liftEffect $ Ref.new true
        let
            sendToBIfFlagIsOn :: chreprB -> m Unit
            sendToBIfFlagIsOn reprB = do -- TODO: Monad.whenM
                flagOn <- liftEffect $ Ref.read flagRef
                if flagOn then do
                  sendIn inletB reprB nodeB
                --   run nodeB
                else pure unit
        SignalX.runSignal $ subscribeOutlet outletA nodeA ~> fromMaybe HF.fallback ~> convertRepr ~> sendToBIfFlagIsOn
        (mbReprA :: Maybe chreprA) <- atOutlet outletA nodeA
        sendToBIfFlagIsOn $ convertRepr $ fromMaybe HF.fallback mbReprA
        pure $ RawLink.make nodeAId outletA inletB nodeBId $ Ref.write false flagRef


disconnect
    :: forall m stateA stateB chreprA chreprB mp
     . MonadEffect m
    => Raw.Link
    -> Node stateA chreprA mp
    -> Node stateB chreprB mp
    -> m Boolean
disconnect link (Node nodeAId _ _ _ _) (Node nodeBId _ _ _ _) =
    if (RawLink.fromNode link == nodeAId) && (RawLink.toNode link == nodeBId) then
        liftEffect (RawLink.cancel link) >>= (const $ pure true)
    else pure false


{- Convert -}


orderInlets :: forall chrepr. Raw.Shape -> InletsValues chrepr -> OrderedInletsValues chrepr
orderInlets shape ivalues = Map.fromFoldable $ resortF $ Map.toUnfoldable $ ivalues
  where
    maxN = Array.length $ RawShape.inlets shape
    resortF :: Array (Id.InletR /\ chrepr) -> Array ((Int /\ Id.InletR) /\ chrepr)
    resortF = map (\(inletR /\ repr) -> ((fromMaybe maxN $ _.order <$> Map.lookup inletR ishapeMap) /\ inletR) /\ repr)
    ishapeMap = Map.fromFoldable $ (\v -> v.name /\ v) <$> RawShape.inlets shape


orderOutlets :: forall chrepr. Raw.Shape -> OutletsValues chrepr -> OrderedOutletsValues chrepr
orderOutlets shape ovalues = Map.fromFoldable $ resortF $ Map.toUnfoldable $ ovalues
  where
    maxN = Array.length $ RawShape.outlets shape
    resortF :: Array (Id.OutletR /\ chrepr) -> Array ((Int /\ Id.OutletR) /\ chrepr)
    resortF = map (\(outletR /\ repr) -> ((fromMaybe maxN $ _.order <$> Map.lookup outletR oshapeMap) /\ outletR) /\ repr)
    oshapeMap = Map.fromFoldable $ (\v -> v.name /\ v) <$> RawShape.outlets shape


toReprableState :: forall state strepr chrepr m. StRepr state strepr => Node state chrepr m -> Node strepr chrepr m
toReprableState (Node nodeR shape tracker protocol fn) =
    Node
        nodeR
        shape
        (RawTracker.toReprableState tracker)
        (RawProtocol.toReprableState protocol)
        $ RawFn.toReprableState fn