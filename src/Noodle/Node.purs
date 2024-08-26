module Noodle.Node where

import Prelude

import Prim.RowList as RL

import Control.Monad.Rec.Class (class MonadRec)

import Effect.Class (class MonadEffect, liftEffect)

import Effect.Console as Console

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.UniqueHash (generate) as UH
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class ToReprRow, class FromReprRow, class HasFallback, class ToRepr)
import Data.Tuple (snd) as Tuple
import Type.Proxy (Proxy(..))
import Record (get, set) as Record
import Record.Extra (keys, class Keys) as Record

import Signal (Signal, (~>))
import Signal.Extra (runInSignal, runSignal) as SignalX

import Noodle.Id as Id
import Noodle.Node.Has (class HasInlet, class HasOutlet)

import Noodle.Fn (Fn, RawFn)
import Noodle.Fn (make, run, run', toRaw) as Fn
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (Raw, reflect, inletRName, outletRName) as Shape
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol (make, getInlets, getOutlets, getRecInlets, getRecOutlets, _sendIn, _sendOut, _unsafeSendIn, _unsafeSendOut) as Protocol
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.Tracker (Tracker) as Tracker
import Noodle.Fn.Updates (ChangeFocus(..)) as Fn
import Noodle.Fn.Process (ProcessM)
import Noodle.Fn.Tracker (Tracker) as Tracker
import Noodle.Fn.Raw.Protocol (Protocol) as Raw
import Noodle.Fn.Raw.Tracker (Tracker) as Raw

import Noodle.Fn.RawToRec as ReprCnv
import Noodle.Wiring (class Wiring)


data Node (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Shape.Raw
        (Tracker state is os repr)
        (Protocol state is os repr)
        (Fn state is os repr m)


data RawNode (state :: Type) (repr :: Type) (m :: Type -> Type)
    = RawNode
        Id.NodeR
        Shape.Raw
        (Raw.Tracker state repr)
        (Raw.Protocol state repr)
        (RawFn state repr m)



type Process :: Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> Type
type Process state is os repr m = ProcessM state is os repr m Unit


{- Making -}


make
    :: forall f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) repr m
     . IsSymbol f
    => InletsDefs inlets => OutletsDefs outlets
    => ToReprRow isrl is Id.InletR repr => ToReprRow osrl os Id.OutletR repr
    => ContainsAllInlets is inlets => ContainsAllOutlets os outlets
    => MonadEffect m
    => Id.Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> Process state is os repr m
    -> m (Node f state is os repr m)
make family state shape inletsRec outletsRec process =
    makeRaw
        (Id.familyR family)
        state
        (Shape.reflect shape)
        (ReprCnv.fromRec Id.inletR inletsRec)
        (ReprCnv.fromRec Id.outletR outletsRec)
        process


makeRaw
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Shape.Raw
    -> Map Id.InletR repr
    -> Map Id.OutletR repr
    -> Process state is os repr m
    -> m (Node f state is os repr m)
makeRaw family state rawShape inletsMap outletsMap process = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeRaw family uniqueHash
    tracker /\ protocol <- Protocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol $ Fn.make (Id.family family) process


{- To Raw -}




{- Running -}



-- TODO: private
runOnInletUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
    .  Wiring m
    => HasFallback repr
    => Node f state is os repr m
    -> m Unit
runOnInletUpdates node =
  SignalX.runSignal $ subscribeInlets node ~> const (run node)


-- TODO: private
runOnStateUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
    .  Wiring m
    => HasFallback repr
    => Node f state is os repr m
    -> m Unit
runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
listenUpdatesAndRun
  :: forall f state (is :: Row Type) (os :: Row Type) repr m
   . Wiring m
  => HasFallback repr
  => Node f state is os repr m
  -> m Unit
listenUpdatesAndRun node = do
  runOnInletUpdates node
  runOnStateUpdates node -- may be running on state updates is not needed
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall f state is os repr m. MonadRec m => MonadEffect m => HasFallback repr => Node f state is os repr m -> m Unit
run (Node _ _ _ protocol fn) = Fn.run' protocol fn



{- Subscriptions -}

-- ToDO: with HasInlet / HasOutlet


subscribeInlet :: forall f state is os repr m. Id.InletR -> Node f state is os repr m -> Signal (Maybe repr)
subscribeInlet input node = Map.lookup input <$> subscribeInlets node


subscribeInletRec :: forall f state is isrl os repr m din. RL.RowToList is isrl => FromReprRow isrl is repr => (Record is -> din) -> Node f state is os repr m -> Signal din
subscribeInletRec fn node = fn <$> subscribeInletsRec node


subscribeInlets :: forall f state is os repr m. Node f state is os repr m -> Signal (Map Id.InletR repr)
subscribeInlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


subscribeInletsRec :: forall f state is isrl os repr m. RL.RowToList is isrl => FromReprRow isrl is repr => Node f state is os repr m -> Signal (Record is)
subscribeInletsRec (Node _ _ tracker _ _) = ReprCnv.toRec Shape.inletRName <$> Tuple.snd <$> tracker.inlets


subscribeOutlet :: forall f state is os repr m. Id.OutletR -> Node f state is os repr m -> Signal (Maybe repr)
subscribeOutlet output node = Map.lookup output <$> subscribeOutlets node


subscribeOutletRec :: forall f state is os osrl repr m dout. RL.RowToList os osrl => FromReprRow osrl os repr => (Record os -> dout) -> Node f state is os repr m -> Signal dout
subscribeOutletRec fn node = fn <$> subscribeOutletsRec node


subscribeOutlets :: forall f state is os repr m. Node f state is os repr m -> Signal (Map Id.OutletR repr)
subscribeOutlets (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeOutletsRec :: forall f state is os osrl repr m. RL.RowToList os osrl => FromReprRow osrl os repr => Node f state is os repr m -> Signal (Record os)
subscribeOutletsRec (Node _ _ tracker _ _) = ReprCnv.toRec Shape.outletRName <$> Tuple.snd <$> tracker.outlets


subscribeState :: forall f state is os repr m. Node f state is os repr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall f state is os repr m. Node f state is os repr m -> Signal (Fn.ChangeFocus /\ state /\ Map Id.InletR repr /\ Map Id.OutletR repr)
subscribeChanges (Node _ _ tracker _ _) = tracker.all


subscribeChangesRec
    :: forall f state is isrl os osrl repr m
     . RL.RowToList is isrl => FromReprRow isrl is repr
    => RL.RowToList os osrl => FromReprRow osrl os repr
    => Node f state is os repr m
    -> Signal (Fn.ChangeFocus /\ state /\ Record is /\ Record os)
subscribeChangesRec (Node _ _ tracker _ _) =
    tracker.all <#>
        \(focus /\ state /\ inputsMap /\ outputsMap) ->
            focus /\ state /\ ReprCnv.toRec Shape.inletRName inputsMap /\ ReprCnv.toRec Shape.outletRName outputsMap


{- Get Data -}


inlets :: forall f state is isrl os repr m. MonadEffect m => FromReprRow isrl is repr => Node f state is os repr m -> m (Record is)
inlets node = liftEffect $ Protocol.getRecInlets $ _getProtocol node


outlets :: forall f state is os osrl repr m. MonadEffect m => FromReprRow osrl os repr => Node f state is os repr m -> m (Record os)
outlets node = liftEffect $ Protocol.getRecOutlets $ _getProtocol node


inletsRaw :: forall f state is os repr m. MonadEffect m => Node f state is os repr m -> m (Map Id.InletR repr)
inletsRaw node = liftEffect $ Protocol.getInlets $ _getProtocol node


outletsRaw :: forall f state is os repr m. MonadEffect m => Node f state is os repr m -> m (Map Id.OutletR repr)
outletsRaw node = liftEffect $ Protocol.getOutlets $ _getProtocol node


inletsRow :: forall f state is os repr m. MonadEffect m => Node f state is os repr m -> Proxy is
inletsRow _ = Proxy :: _ is


outletsRow :: forall f state is os repr m. MonadEffect m => Node f state is os repr m -> Proxy os
outletsRow _ = Proxy :: _ os


atInlet :: forall f i state is is' isrl os repr m din. MonadEffect m => FromReprRow isrl is repr => HasInlet is is' i din => Id.Inlet i -> Node f state is os repr m -> m din
atInlet _ node = inlets node <#> Record.get (Proxy :: _ i)


atOutlet :: forall f o state is os os' osrl repr m dout. MonadEffect m => FromReprRow osrl os repr => HasOutlet os os' o dout => Id.Outlet o -> Node f state is os repr m -> m dout
atOutlet _ node = outlets node <#> Record.get (Proxy :: _ o)


atInletR :: forall f state is os repr m. MonadEffect m => Id.InletR -> Node f state is os repr m -> m (Maybe repr)
atInletR iid node = inletsRaw node <#> Map.lookup iid


atOutletR :: forall f state is os repr m. MonadEffect m => Id.OutletR -> Node f state is os repr m -> m (Maybe repr)
atOutletR oid node = outletsRaw node <#> Map.lookup oid


{- Send data -}


sendIn :: forall f i state is is' os repr m din. MonadEffect m => ToRepr din repr => HasInlet is is' i din  => ToRepr din repr => Id.Inlet i -> din -> Node f state is os repr m -> m Unit
sendIn input din = liftEffect <<< Protocol._sendIn input din <<< _getProtocol


sendOut :: forall f o state is os os' repr m dout. MonadEffect m => ToRepr dout repr => HasOutlet os os' o dout => Id.Outlet o -> dout -> Node f state is os repr m -> m Unit
sendOut output dout = liftEffect <<< Protocol._sendOut output dout <<< _getProtocol


unsafeSendIn :: forall f state is os repr m. MonadEffect m => Id.InletR -> repr -> Node f state is os repr m -> m Unit
unsafeSendIn input repr = liftEffect <<< Protocol._unsafeSendIn input repr <<< _getProtocol


unsafeSendOut :: forall f state is os repr m. MonadEffect m => Id.OutletR -> repr -> Node f state is os repr m -> m Unit
unsafeSendOut output repr = liftEffect <<< Protocol._unsafeSendOut output repr <<< _getProtocol


{- Private accessors -}


_getProtocol :: forall f state is os repr m. Node f state is os repr m -> Protocol state is os repr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall f state is os repr m. Node f state is os repr m -> Tracker state is os repr
_getTracker (Node _ _ tracker _ _) = tracker


{- Rawify -}


toRaw :: forall f state is os repr m. Node f state is os repr m -> RawNode state repr m
toRaw (Node nodeR shape tracker protocol fn) = RawNode nodeR shape tracker protocol $ Fn.toRaw fn
