module Noodle.Node where

import Prelude

import Type.Proxy (Proxy(..))

import Prim.RowList as RL

import Control.Monad.Rec.Class (class MonadRec)

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write) as Ref

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.UniqueHash (generate) as UH
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Record (get) as Record

import Signal (Signal, (~>))
import Signal.Extra (runSignal) as SignalX

import Noodle.Id (Inlet, Outlet, Family(..), NodeR, InletR, OutletR, FamilyR, family, familyR, inletR, outletR, nodeR_) as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make, run', toRaw) as Fn
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Fn.Process (Process)
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol (make, getInlets, getOutlets, getRecInlets, getRecOutlets, getState, _sendIn, _sendOut, _unsafeSendIn, _unsafeSendOut, modifyState) as Protocol
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.Updates (UpdateFocus) as Fn
import Noodle.Fn.Updates (toTuple) as Updates
-- import Noodle.Fn.Process (ProcessM)
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (inletRName, outletRName) as RShape
import Noodle.Raw.FromToRec as ReprCnv
import Noodle.Repr (class ToReprRow, class FromRepr, class FromReprRow, class HasFallback, class ToRepr)
import Noodle.Repr (fallback, inbetween, inbetween') as Repr
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Link (Link)
import Noodle.Link (fromRaw, fromNode, toNode, cancel) as Link
import Noodle.Raw.Node (Node(..), InletsValues, OutletsValues, NodeChanges) as Raw
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (make) as RawLink
import Noodle.Wiring (class Wiring)


data Node (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Tracker state is os repr)
        (Protocol state is os repr)
        (Fn state is os repr m)


{- Get info -}


id :: forall f state is os repr m. Node f state is os repr m -> Id.NodeR
id (Node id _ _ _ _) = id


family :: forall f state is os repr m. Node f state is os repr m -> Id.Family f
family _ = (Id.Family :: _ f)


{- Making -}


make
    :: forall m f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) repr mp
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
    -> Process state is os repr mp
    -> m (Node f state is os repr mp)
make family state shape inletsRec outletsRec process =
    make_
        (Id.familyR family)
        state
        (Shape.reflect shape)
        (ReprCnv.fromRec Id.inletR inletsRec)
        (ReprCnv.fromRec Id.outletR outletsRec)
        process


make_ -- TODO: private
    :: forall m f state (is :: Row Type) (os :: Row Type) repr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> Raw.InletsValues repr
    -> Raw.OutletsValues repr
    -> Process state is os repr mp
    -> m (Node f state is os repr mp)
make_ family state rawShape inletsMap outletsMap process = do
    _makeWithFn family state rawShape inletsMap outletsMap $ Fn.make (Id.family family) process


_makeWithFn -- TODO: private
    :: forall m f state (is :: Row Type) (os :: Row Type) repr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> Raw.InletsValues repr
    -> Raw.OutletsValues repr
    -> Fn state is os repr mp
    -> m (Node f state is os repr mp)
_makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeR_ family uniqueHash
    tracker /\ protocol <- Protocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn


{- Running -}

-- TODO: Try distinguishing outer monad from inner one here as well (as we did for other methods)
--       Could be not possible because running the node' processing function requires the same monad environment

-- TODO: private
_runOnInletUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
    .  Wiring m
    => HasFallback repr
    => Node f state is os repr m
    -> m Unit
_runOnInletUpdates node =
  SignalX.runSignal $ subscribeInletsRaw node ~> const (run node)


-- TODO: private
_runOnStateUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) repr m
    .  Wiring m
    => HasFallback repr
    => Node f state is os repr m
    -> m Unit
_runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
_listenUpdatesAndRun
  :: forall f state (is :: Row Type) (os :: Row Type) repr m
   . Wiring m
  => HasFallback repr
  => Node f state is os repr m
  -> m Unit
_listenUpdatesAndRun node = do
  _runOnInletUpdates node
  -- this leading us into a loop when `modifyState` is inside the `Node`' process call
  --runOnStateUpdates node -- may be running on state updates is not needed;
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall f state is os repr m. MonadRec m => MonadEffect m => HasFallback repr => Node f state is os repr m -> m Unit
run (Node _ _ _ protocol fn) = Fn.run' protocol fn



{- Subscriptions -}


subscribeInletR :: forall f state is os repr m. Id.InletR -> Node f state is os repr m -> Signal (Maybe repr)
subscribeInletR input node = Map.lookup input <$> subscribeInletsRaw node


subscribeInlet :: forall f i state is is' isrl os repr m din. RL.RowToList is isrl => FromReprRow isrl is repr => HasInlet is is' i din => Id.Inlet i -> Node f state is os repr m -> Signal din
subscribeInlet _ = subscribeInlet_ $ Record.get (Proxy :: _ i)


subscribeInlet_ :: forall f state is isrl os repr m din. RL.RowToList is isrl => FromReprRow isrl is repr => (Record is -> din) -> Node f state is os repr m -> Signal din
subscribeInlet_ fn node = fn <$> subscribeInlets node


subscribeInletsRaw :: forall f state is os repr m. Node f state is os repr m -> Signal (Raw.InletsValues repr)
subscribeInletsRaw (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


subscribeInlets :: forall f state is isrl os repr m. RL.RowToList is isrl => FromReprRow isrl is repr => Node f state is os repr m -> Signal (Record is)
subscribeInlets (Node _ _ tracker _ _) = ReprCnv.toRec RShape.inletRName <$> Tuple.snd <$> tracker.inlets


subscribeOutletR :: forall f state is os repr m. Id.OutletR -> Node f state is os repr m -> Signal (Maybe repr)
subscribeOutletR output node = Map.lookup output <$> subscribeOutletsRaw node


subscribeOutlet :: forall f o state is os os' osrl repr m dout. RL.RowToList os osrl => FromReprRow osrl os repr => HasOutlet os os' o dout => Id.Outlet o -> Node f state is os repr m -> Signal dout
subscribeOutlet _ = subscribeOutlet_ $ Record.get (Proxy :: _ o)


subscribeOutlet_ :: forall f state is os osrl repr m dout. RL.RowToList os osrl => FromReprRow osrl os repr => (Record os -> dout) -> Node f state is os repr m -> Signal dout
subscribeOutlet_ fn node = fn <$> subscribeOutlets node


subscribeOutletsRaw :: forall f state is os repr m. Node f state is os repr m -> Signal (Raw.OutletsValues repr)
subscribeOutletsRaw (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeOutlets :: forall f state is os osrl repr m. RL.RowToList os osrl => FromReprRow osrl os repr => Node f state is os repr m -> Signal (Record os)
subscribeOutlets (Node _ _ tracker _ _) = ReprCnv.toRec RShape.outletRName <$> Tuple.snd <$> tracker.outlets


subscribeState :: forall f state is os repr m. Node f state is os repr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall f state is os repr m. Node f state is os repr m -> Signal (Raw.NodeChanges state repr)
subscribeChanges (Node _ _ tracker _ _) = tracker.all <#> Updates.toTuple


subscribeChangesRec
    :: forall f state is isrl os osrl repr m
     . RL.RowToList is isrl => FromReprRow isrl is repr
    => RL.RowToList os osrl => FromReprRow osrl os repr
    => Node f state is os repr m
    -> Signal (Fn.UpdateFocus /\ state /\ Record is /\ Record os)
subscribeChangesRec (Node _ _ tracker _ _) =
    tracker.all <#> Updates.toTuple <#>
        \(focus /\ state /\ inputsMap /\ outputsMap) ->
            focus /\ state /\ ReprCnv.toRec RShape.inletRName inputsMap /\ ReprCnv.toRec RShape.outletRName outputsMap


{- Get Data -}


shape :: forall f state is os repr m. Node f state is os repr m -> Raw.Shape
shape (Node _ shape _ _ _) = shape


inlets :: forall m f state is isrl os repr mp. MonadEffect m => FromReprRow isrl is repr => Node f state is os repr mp -> m (Record is)
inlets node = liftEffect $ Protocol.getRecInlets $ _getProtocol node


outlets :: forall m f state is os osrl repr mp. MonadEffect m => FromReprRow osrl os repr => Node f state is os repr mp -> m (Record os)
outlets node = liftEffect $ Protocol.getRecOutlets $ _getProtocol node


inletsRaw :: forall m f state is os repr mp. MonadEffect m => Node f state is os repr mp -> m (Raw.InletsValues repr)
inletsRaw node = liftEffect $ Protocol.getInlets $ _getProtocol node


outletsRaw :: forall m f state is os repr mp. MonadEffect m => Node f state is os repr mp -> m (Raw.OutletsValues repr)
outletsRaw node = liftEffect $ Protocol.getOutlets $ _getProtocol node


inletsRow :: forall f state is os repr m. Node f state is os repr m -> Proxy is
inletsRow _ = Proxy :: _ is


outletsRow :: forall f state is os repr m. Node f state is os repr m -> Proxy os
outletsRow _ = Proxy :: _ os


state :: forall m f state is os repr mp. MonadEffect m => Node f state is os repr mp -> m state
state node = liftEffect $ Protocol.getState $ _getProtocol node


-- TODO: useful operators for functions below (flipped)

infixr 6 atInletFlipped as <-#
infixr 6 atOutletFlipped as <-@

infixr 6 getFromInletsFlipped as <=#
infixr 6 getFromOutletsFlipped as <=@


atInlet :: forall m f i state is is' isrl os repr mp din. MonadEffect m => FromReprRow isrl is repr => HasInlet is is' i din => Id.Inlet i -> Node f state is os repr mp -> m din
atInlet _ = getFromInlets $ Record.get (Proxy :: _ i)


atInletFlipped :: forall m f i state is is' isrl os repr mp din. MonadEffect m => FromReprRow isrl is repr => HasInlet is is' i din => Node f state is os repr mp -> Id.Inlet i -> m din
atInletFlipped = flip atInlet


atOutlet :: forall m f o state is os os' osrl repr mp dout. MonadEffect m => FromReprRow osrl os repr => HasOutlet os os' o dout => Id.Outlet o -> Node f state is os repr mp -> m dout
atOutlet _ = getFromOutlets $ Record.get (Proxy :: _ o)


atOutletFlipped :: forall m f o state is os os' osrl repr mp dout. MonadEffect m => FromReprRow osrl os repr => HasOutlet os os' o dout => Node f state is os repr mp -> Id.Outlet o -> m dout
atOutletFlipped = flip atOutlet


atInletR :: forall m f state is os repr mp. MonadEffect m => Id.InletR -> Node f state is os repr mp -> m (Maybe repr)
atInletR iid node = inletsRaw node <#> Map.lookup iid


atOutletR :: forall m f state is os repr mp. MonadEffect m => Id.OutletR -> Node f state is os repr mp -> m (Maybe repr)
atOutletR oid node = outletsRaw node <#> Map.lookup oid


getFromInlets :: forall m f state is isrl os repr mp din. MonadEffect m => FromReprRow isrl is repr => (Record is -> din) -> Node f state is os repr mp -> m din
getFromInlets getter node = inlets node <#> getter


getFromInletsFlipped :: forall m f state is isrl os repr mp din. MonadEffect m => FromReprRow isrl is repr => Node f state is os repr mp -> (Record is -> din) -> m din
getFromInletsFlipped = flip getFromInlets


getFromOutlets :: forall m f state is os osrl repr mp dout. MonadEffect m => FromReprRow osrl os repr => (Record os -> dout) -> Node f state is os repr mp -> m dout
getFromOutlets getter node = outlets node <#> getter


getFromOutletsFlipped :: forall m f state is os osrl repr mp dout. MonadEffect m => FromReprRow osrl os repr => Node f state is os repr mp -> (Record os -> dout) -> m dout
getFromOutletsFlipped = flip getFromOutlets


{- Send data -}


infixr 6 sendInOp as #->
infixr 6 sendOutOp as @->


sendIn :: forall m f i state is is' os repr mp din. MonadEffect m => ToRepr din repr => HasInlet is is' i din  => Id.Inlet i -> din -> Node f state is os repr mp -> m Unit
sendIn input din = liftEffect <<< Protocol._sendIn input din <<< _getProtocol


sendInOp :: forall m f i state is is' os repr mp din. MonadEffect m => ToRepr din repr => HasInlet is is' i din  => Node f state is os repr mp -> Id.Inlet i /\ din -> m Unit
sendInOp node (input /\ din) = sendIn input din node


sendOut :: forall m f o state is os os' repr mp dout. MonadEffect m => ToRepr dout repr => HasOutlet os os' o dout => Id.Outlet o -> dout -> Node f state is os repr mp -> m Unit
sendOut output dout = liftEffect <<< Protocol._sendOut output dout <<< _getProtocol


sendOutOp :: forall m f o state is os os' repr mp dout. MonadEffect m => ToRepr dout repr => HasOutlet os os' o dout => Node f state is os repr mp -> Id.Outlet o /\ dout -> m Unit
sendOutOp node (output /\ dout) = sendOut output dout node


unsafeSendIn :: forall m f state is os repr mp. MonadEffect m => Id.InletR -> repr -> Node f state is os repr mp -> m Unit
unsafeSendIn input repr = liftEffect <<< Protocol._unsafeSendIn input repr <<< _getProtocol


unsafeSendOut :: forall m f state is os repr mp. MonadEffect m => Id.OutletR -> repr -> Node f state is os repr mp -> m Unit
unsafeSendOut output repr = liftEffect <<< Protocol._unsafeSendOut output repr <<< _getProtocol


modifyState :: forall m f state is os repr mp. MonadEffect m => (state -> state) -> Node f state is os repr mp -> m Unit
modifyState f = liftEffect <<< Protocol.modifyState f <<< _getProtocol


{- Connecting -}


infixr 6 connectOp as <~>


connect
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' repr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromRepr repr doutA
    => ToRepr dinB repr
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA repr mp
    -> Node fB stateB isB osB repr mp
    -> m (Link fA fB oA iB)
connect outletA inletB nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) identity nodeA nodeB <#> Link.fromRaw


connectOp
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' repr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromRepr repr doutA
    => ToRepr dinB repr
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Node fA stateA isA osA repr mp /\ Id.Outlet oA
    -> Node fB stateB isB osB repr mp /\ Id.Inlet iB
    -> m (Link fA fB oA iB)
connectOp (nodeA /\ outletA) (nodeB /\ inletB) =
    connect outletA inletB nodeA nodeB


connectBySameRepr
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' repr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => ToRepr doutA repr
    => FromRepr repr dinB
    => Proxy repr -- FIXME: Proxy is not needed anymore
    -> Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA repr mp
    -> Node fB stateB isB osB repr mp
    -> m (Link fA fB oA iB)
connectBySameRepr _ outletA inletB nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) identity nodeA nodeB <#> Link.fromRaw


connectByDistinctRepr
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' reprA reprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromRepr reprA doutA
    => ToRepr dinB reprB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Id.Outlet oA
    -> Id.Inlet iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA reprA mp
    -> Node fB stateB isB osB reprB mp
    -> m (Link fA fB oA iB)
connectByDistinctRepr outletA inletB convertF nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) (Repr.inbetween convertF) nodeA nodeB <#> Link.fromRaw



connectAlike
    :: forall m fA fB oA iB d stateA stateB isA isB isB' osA osB osA' reprA reprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => HasFallback reprB
    => FromRepr reprA d
    => ToRepr d reprB
    => HasOutlet osA osA' oA d
    => HasInlet isB isB' iB d
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA reprA mp
    -> Node fB stateB isB osB reprB mp
    -> m (Link fA fB oA iB)
connectAlike outletA inletB nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) (Repr.inbetween' (Proxy :: _ d)) nodeA nodeB <#> Link.fromRaw


unsafeConnect
    :: forall m fA fB stateA stateB isA isB osA osB reprA reprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => HasFallback reprA
    => HasFallback reprB
    => Id.OutletR
    -> Id.InletR
    -> (reprA -> reprB)
    -> Node fA stateA isA osA reprA mp
    -> Node fB stateB isB osB reprB mp
    -> m Raw.Link
unsafeConnect
    outletA
    inletB
    convertRepr
    nodeA@(Node nodeAId _ _ _ _)
    nodeB@(Node nodeBId _ _ _ _) =
    do
        flagRef <- liftEffect $ Ref.new true
        let
            sendToBIfFlagIsOn :: reprB -> m Unit
            sendToBIfFlagIsOn reprB = do -- TODO: Monad.whenM
                flagOn <- liftEffect $ Ref.read flagRef
                if flagOn then do
                  unsafeSendIn inletB reprB nodeB
                --   run nodeB
                else pure unit
        SignalX.runSignal $ subscribeOutletR outletA nodeA ~> fromMaybe Repr.fallback ~> convertRepr ~> sendToBIfFlagIsOn
        (mbReprA :: Maybe reprA) <- atOutletR outletA nodeA
        sendToBIfFlagIsOn $ convertRepr $ fromMaybe Repr.fallback mbReprA
        pure $ RawLink.make nodeAId outletA inletB nodeBId $ Ref.write false flagRef


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' reprA reprB mo mi
     . MonadEffect mo
    => IsSymbol fA
    => IsSymbol fB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Link fA fB oA iB
    -> Node fA stateA isA osA reprA mi
    -> Node fB stateB isB osB reprB mi
    -> mo Boolean
disconnect link (Node nodeAId _ _ _ _) (Node nodeBId _ _ _ _) =
    if (Link.fromNode link == nodeAId) && (Link.toNode link == nodeBId) then
        liftEffect (Link.cancel link) >>= (const $ pure true)
    else pure false



{- Private accessors -}


_getProtocol :: forall f state is os repr m. Node f state is os repr m -> Protocol state is os repr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall f state is os repr m. Node f state is os repr m -> Tracker state is os repr
_getTracker (Node _ _ tracker _ _) = tracker


{- Utils -}


logUpdates :: forall f state is os repr m. Show state => Show repr => Node f state is os repr m -> Signal String
logUpdates (Node _ _ tracker _ _) = show <$> tracker.all



{- Rawify -}


toRaw :: forall f state is os repr m. Node f state is os repr m -> Raw.Node state repr m
toRaw (Node nodeR shape tracker protocol fn) =
    Raw.Node
        nodeR
        shape
        tracker
        protocol
        $ Fn.toRaw fn