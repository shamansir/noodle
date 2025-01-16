module Noodle.Node where

import Prelude

import Type.Proxy (Proxy(..))

import Prim.RowList as RL

import Control.Monad.Rec.Class (class MonadRec)

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write) as Ref

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (lookup, toUnfoldable) as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.UniqueHash (generate) as UH
import Data.Tuple (uncurry, curry)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Record (get) as Record

import Signal (Signal, (~>))
import Signal.Extra (runSignal) as SignalX

import Noodle.Id (Inlet, Outlet, Family(..), NodeR, InletR, OutletR, FamilyR, family, familyR, inletR, outletR, nodeR_, inletRName, outletRName) as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make, run', toRaw) as Fn
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Fn.ToFn (Fn, Argument, Output, arg, out) as ToFn
import Noodle.Fn.Process (Process)
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol (make, getInlets, getOutlets, getRecInlets, getRecOutlets, getState, _sendIn, _sendOut, _unsafeSendIn, _unsafeSendOut, modifyState) as Protocol
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.Updates (UpdateFocus, InletsUpdate(..)) as Fn
import Noodle.Fn.Updates (toRecord) as Updates
-- import Noodle.Fn.Process (ProcessM)
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (inletRName, outletRName, hasHotInlets, isHotInlet) as RawShape
import Noodle.Raw.Fn.Updates (toFn) as RawUpdates
import Noodle.Raw.FromToRec as ChReprCnv
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel, class ToValueInChannel, class ToValuesInChannelRow, class FromValuesInChannelRow, fromValueInChannel, toValueInChannel)
import Noodle.Repr.ValueInChannel (inbetween, inbetween', toMaybe, accept, inbetweenB, toFallback, _reportMissingKey) as ViC
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Link (Link)
import Noodle.Link (fromRaw, fromNode, toNode, cancel) as Link
import Noodle.Raw.Node (Node(..), InletsValues, InitialInletsValues, OutletsValues, InitialOutletsValues, NodeChanges, orderInlets, orderOutlets) as Raw
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (make) as RawLink
import Noodle.Wiring (class Wiring)


data Node (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (chrepr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Tracker state is os chrepr)
        (Protocol state is os chrepr)
        (Fn state is os chrepr m)


{- Get info -}


id :: forall f state is os chrepr m. Node f state is os chrepr m -> Id.NodeR
id (Node id _ _ _ _) = id


family :: forall f state is os chrepr m. Node f state is os chrepr m -> Id.Family f
family _ = (Id.Family :: _ f)


{- Making -}


make
    :: forall m f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Outlets) chrepr mp
     . IsSymbol f
    => InletsDefs inlets => OutletsDefs outlets
    => HasFallback chrepr
    => FromValuesInChannelRow isrl is Id.InletR chrepr
    => FromValuesInChannelRow osrl os Id.OutletR chrepr
    => ContainsAllInlets is inlets => ContainsAllOutlets os outlets
    => MonadEffect m
    => Id.Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> Process state is os chrepr mp
    -> m (Node f state is os chrepr mp)
make family state shape inletsRec outletsRec process =
    make_
        (Id.familyR family)
        state
        (Shape.reflect shape)
        (ViC.toFallback <$> ChReprCnv.fromRec Id.inletR inletsRec) -- FIXME: may be could manage without `fallback`` here?
        (ViC.toFallback <$> ChReprCnv.fromRec Id.outletR outletsRec) -- FIXME: may be could manage without `fallback`` here?
        process


make_ -- TODO: private
    :: forall m f state (is :: Row Type) (os :: Row Type) chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> Raw.InitialInletsValues  chrepr
    -> Raw.InitialOutletsValues chrepr
    -> Process state is os chrepr mp
    -> m (Node f state is os chrepr mp)
make_ family state rawShape inletsMap outletsMap process = do
    _makeWithFn family state rawShape inletsMap outletsMap $ Fn.make (Id.family family) process


_makeWithFn -- TODO: private
    :: forall m f state (is :: Row Type) (os :: Row Type) chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> Raw.Shape
    -> Raw.InitialInletsValues  chrepr
    -> Raw.InitialOutletsValues chrepr
    -> Fn state is os chrepr mp
    -> m (Node f state is os chrepr mp)
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
    :: forall f state (is :: Row Type) (os :: Row Type) chrepr m
    .  Wiring m
    => HasFallback chrepr
    => Node f state is os chrepr m
    -> m Unit
_runOnInletUpdates node =
    -- FIXME: use the same method from RawNode
    SignalX.runSignal $ _subscribeInletsRaw node ~> isHotUpdate ~> runOnlyIfHasHotInlet
    where
        runOnlyIfHasHotInlet isHot = if isHot then run node else pure unit
        isHotUpdate = Tuple.fst >>> case _ of
            Fn.AllInlets          -> RawShape.hasHotInlets $ shape node
            Fn.SingleInlet inletR -> fromMaybe false $ RawShape.isHotInlet inletR $ shape node


-- TODO: private
-- FIXME: running this function with a node that modifies or even gets its state in processing function
--        cases Maximum Stack exeeded error, at least in Blessed. Could be because we store state in a `Ref`,
--        could be something else... May be this method has no sense at all...
_runOnStateUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) chrepr m
    .  Wiring m
    => HasFallback chrepr
    => Node f state is os chrepr m
    -> m Unit
_runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)



--- FIXME: find better name
_listenUpdatesAndRun
  :: forall f state (is :: Row Type) (os :: Row Type) chrepr m
   . Wiring m
  => HasFallback chrepr
  => Node f state is os chrepr m
  -> m Unit
_listenUpdatesAndRun node = do
  _runOnInletUpdates node
  -- this leading us into a loop when `modifyState` is inside the `Node`' process call
  --runOnStateUpdates node -- may be running on state updates is not needed;
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


run :: forall f state is os chrepr m. MonadRec m => MonadEffect m => HasFallback chrepr => Node f state is os chrepr m -> m Unit
run (Node _ _ _ protocol fn) = Fn.run' protocol fn



{- Subscriptions -}


subscribeInletR :: forall f state is os chrepr m. Id.InletR -> Node f state is os chrepr m -> Signal (ValueInChannel chrepr)
subscribeInletR inletR node = (ViC._reportMissingKey $ Id.inletRName inletR) <$> Map.lookup inletR <$> subscribeInletsRaw node


subscribeInlet :: forall f i state is is' isrl os chrepr m din. RL.RowToList is isrl => ToValuesInChannelRow isrl is chrepr => HasInlet is is' i din => Id.Inlet i -> Node f state is os chrepr m -> Signal din
subscribeInlet _ = subscribeInlet_ $ Record.get (Proxy :: _ i)


subscribeInlet_ :: forall f state is isrl os chrepr m din. RL.RowToList is isrl => ToValuesInChannelRow isrl is chrepr => (Record is -> din) -> Node f state is os chrepr m -> Signal din
subscribeInlet_ fn node = fn <$> subscribeInlets node


subscribeInletsRaw :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal (Raw.InletsValues chrepr)
subscribeInletsRaw (Node _ _ tracker _ _) = Tuple.snd <$> tracker.inlets


_subscribeInletsRaw :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal (Fn.InletsUpdate /\ Raw.InletsValues chrepr)
_subscribeInletsRaw (Node _ _ tracker _ _) = tracker.inlets


subscribeInlets :: forall f state is isrl os chrepr m. RL.RowToList is isrl => ToValuesInChannelRow isrl is chrepr => Node f state is os chrepr m -> Signal (Record is)
subscribeInlets (Node _ _ tracker _ _) = ChReprCnv.toRec RawShape.inletRName <$> Tuple.snd <$> tracker.inlets


subscribeOutletR :: forall f state is os chrepr m. Id.OutletR -> Node f state is os chrepr m -> Signal (ValueInChannel chrepr)
subscribeOutletR outletR node = (ViC._reportMissingKey $ Id.outletRName outletR) <$> Map.lookup outletR <$> subscribeOutletsRaw node


subscribeOutlet :: forall f o state is os os' osrl chrepr m dout. RL.RowToList os osrl => ToValuesInChannelRow osrl os chrepr => HasOutlet os os' o dout => Id.Outlet o -> Node f state is os chrepr m -> Signal dout
subscribeOutlet _ = subscribeOutlet_ $ Record.get (Proxy :: _ o)


subscribeOutlet_ :: forall f state is os osrl chrepr m dout. RL.RowToList os osrl => ToValuesInChannelRow osrl os chrepr => (Record os -> dout) -> Node f state is os chrepr m -> Signal dout
subscribeOutlet_ fn node = fn <$> subscribeOutlets node


subscribeOutletsRaw :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal (Raw.OutletsValues chrepr)
subscribeOutletsRaw (Node _ _ tracker _ _) = Tuple.snd <$> tracker.outlets


subscribeOutlets :: forall f state is os osrl chrepr m. RL.RowToList os osrl => ToValuesInChannelRow osrl os chrepr => Node f state is os chrepr m -> Signal (Record os)
subscribeOutlets (Node _ _ tracker _ _) = ChReprCnv.toRec RawShape.outletRName <$> Tuple.snd <$> tracker.outlets


subscribeState :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal (Raw.NodeChanges state chrepr)
subscribeChanges (Node _ shape tracker _ _) =
    tracker.all
        <#> Updates.toRecord
        <#> \chs -> chs
        { inlets = Raw.orderInlets shape chs.inlets
        , outlets = Raw.orderOutlets shape chs.outlets
        }


subscribeChangesAsFn :: forall f state is os chrepr m. Node f state is os chrepr m -> Signal (ToFn.Fn (ValueInChannel chrepr) (ValueInChannel chrepr))
subscribeChangesAsFn (Node nodeR _ tracker _ _) = tracker.all <#> RawUpdates.toFn nodeR


{- Get Data -}


shape :: forall f state is os chrepr m. Node f state is os chrepr m -> Raw.Shape
shape (Node _ shape _ _ _) = shape


inlets :: forall m f state is isrl os chrepr mp. MonadEffect m => ToValuesInChannelRow isrl is chrepr => Node f state is os chrepr mp -> m (Record is)
inlets node = liftEffect $ Protocol.getRecInlets $ _getProtocol node


outlets :: forall m f state is os osrl chrepr mp. MonadEffect m => ToValuesInChannelRow osrl os chrepr => Node f state is os chrepr mp -> m (Record os)
outlets node = liftEffect $ Protocol.getRecOutlets $ _getProtocol node


inletsRaw :: forall m f state is os chrepr mp. MonadEffect m => Node f state is os chrepr mp -> m (Raw.InletsValues chrepr)
inletsRaw node = liftEffect $ Protocol.getInlets $ _getProtocol node


outletsRaw :: forall m f state is os chrepr mp. MonadEffect m => Node f state is os chrepr mp -> m (Raw.OutletsValues chrepr)
outletsRaw node = liftEffect $ Protocol.getOutlets $ _getProtocol node


inletsRow :: forall f state is os chrepr m. Node f state is os chrepr m -> Proxy is
inletsRow _ = Proxy :: _ is


outletsRow :: forall f state is os chrepr m. Node f state is os chrepr m -> Proxy os
outletsRow _ = Proxy :: _ os


state :: forall m f state is os chrepr mp. MonadEffect m => Node f state is os chrepr mp -> m state
state node = liftEffect $ Protocol.getState $ _getProtocol node


-- TODO: useful operators for functions below (flipped)

infixr 6 atInletFlipped as <-#
infixr 6 atOutletFlipped as <-@

infixr 6 getFromInletsFlipped as <=#
infixr 6 getFromOutletsFlipped as <=@


atInlet :: forall m f i state is is' isrl os chrepr mp din. MonadEffect m => ToValuesInChannelRow isrl is chrepr => HasInlet is is' i din => Id.Inlet i -> Node f state is os chrepr mp -> m din
atInlet _ = getFromInlets $ Record.get (Proxy :: _ i)


atInletFlipped :: forall m f i state is is' isrl os chrepr mp din. MonadEffect m => ToValuesInChannelRow isrl is chrepr => HasInlet is is' i din => Node f state is os chrepr mp -> Id.Inlet i -> m din
atInletFlipped = flip atInlet


atOutlet :: forall m f o state is os os' osrl chrepr mp dout. MonadEffect m => ToValuesInChannelRow osrl os chrepr => HasOutlet os os' o dout => Id.Outlet o -> Node f state is os chrepr mp -> m dout
atOutlet _ = getFromOutlets $ Record.get (Proxy :: _ o)


atOutletFlipped :: forall m f o state is os os' osrl chrepr mp dout. MonadEffect m => ToValuesInChannelRow osrl os chrepr => HasOutlet os os' o dout => Node f state is os chrepr mp -> Id.Outlet o -> m dout
atOutletFlipped = flip atOutlet


atInletR :: forall m f state is os chrepr mp. MonadEffect m => Id.InletR -> Node f state is os chrepr mp -> m (ValueInChannel chrepr)
atInletR inletR node = inletsRaw node <#> Map.lookup inletR <#> (ViC._reportMissingKey $ Id.inletRName inletR)


atOutletR :: forall m f state is os chrepr mp. MonadEffect m => Id.OutletR -> Node f state is os chrepr mp -> m (ValueInChannel chrepr)
atOutletR outletR node = outletsRaw node <#> Map.lookup outletR <#> (ViC._reportMissingKey $ Id.outletRName outletR)


getFromInlets :: forall m f state is isrl os chrepr mp din. MonadEffect m => ToValuesInChannelRow isrl is chrepr => (Record is -> din) -> Node f state is os chrepr mp -> m din
getFromInlets getter node = inlets node <#> getter


getFromInletsFlipped :: forall m f state is isrl os chrepr mp din. MonadEffect m => ToValuesInChannelRow isrl is chrepr => Node f state is os chrepr mp -> (Record is -> din) -> m din
getFromInletsFlipped = flip getFromInlets


getFromOutlets :: forall m f state is os osrl chrepr mp dout. MonadEffect m => ToValuesInChannelRow osrl os chrepr => (Record os -> dout) -> Node f state is os chrepr mp -> m dout
getFromOutlets getter node = outlets node <#> getter


getFromOutletsFlipped :: forall m f state is os osrl chrepr mp dout. MonadEffect m => ToValuesInChannelRow osrl os chrepr => Node f state is os chrepr mp -> (Record os -> dout) -> m dout
getFromOutletsFlipped = flip getFromOutlets


{- Send data -}


infixr 6 sendInOp as #->
infixr 6 sendOutOp as @->


sendIn :: forall m f i state is is' os chrepr mp din. MonadEffect m => HasFallback chrepr => FromValueInChannel din chrepr => HasInlet is is' i din  => Id.Inlet i -> din -> Node f state is os chrepr mp -> m Unit
sendIn input din = liftEffect <<< Protocol._sendIn input din <<< _getProtocol


sendInOp :: forall m f i state is is' os chrepr mp din. MonadEffect m => HasFallback chrepr => FromValueInChannel din chrepr => HasInlet is is' i din  => Node f state is os chrepr mp -> Id.Inlet i /\ din -> m Unit
sendInOp node (input /\ din) = sendIn input din node


sendOut :: forall m f o state is os os' chrepr mp dout. MonadEffect m => HasFallback chrepr => FromValueInChannel dout chrepr => HasOutlet os os' o dout => Id.Outlet o -> dout -> Node f state is os chrepr mp -> m Unit
sendOut output dout = liftEffect <<< Protocol._sendOut output dout <<< _getProtocol


sendOutOp :: forall m f o state is os os' chrepr mp dout. MonadEffect m => HasFallback chrepr => FromValueInChannel dout chrepr => HasOutlet os os' o dout => Node f state is os chrepr mp -> Id.Outlet o /\ dout -> m Unit
sendOutOp node (output /\ dout) = sendOut output dout node


unsafeSendIn :: forall m f state is os chrepr mp. MonadEffect m => Id.InletR -> ValueInChannel chrepr -> Node f state is os chrepr mp -> m Unit
unsafeSendIn input chrepr = liftEffect <<< Protocol._unsafeSendIn input chrepr <<< _getProtocol


unsafeSendOut :: forall m f state is os chrepr mp. MonadEffect m => Id.OutletR -> ValueInChannel chrepr -> Node f state is os chrepr mp -> m Unit
unsafeSendOut output chrepr = liftEffect <<< Protocol._unsafeSendOut output chrepr <<< _getProtocol


modifyState :: forall m f state is os chrepr mp. MonadEffect m => (state -> state) -> Node f state is os chrepr mp -> m Unit
modifyState f = liftEffect <<< Protocol.modifyState f <<< _getProtocol


setState :: forall m f state is os chrepr mp. MonadEffect m => state -> Node f state is os chrepr mp -> m Unit
setState = modifyState <<< const


{- Connecting -}


infixr 6 connectOp as <~>


connect
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' chrepr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromValueInChannel dinB chrepr
    => ToValueInChannel chrepr dinB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA chrepr mp
    -> Node fB stateB isB osB chrepr mp
    -> m (Link fA fB oA iB)
connect outletA inletB nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) convert2 nodeA nodeB <#> Link.fromRaw
    where
        convert2 :: chrepr -> ValueInChannel chrepr
        convert2 aAsRepr = (toValueInChannel aAsRepr :: ValueInChannel dinB) <#> fromValueInChannel
    -- FIXME: we always use `ChRepr.accept` here...
    -- FIXME: same as `connectBySameRepr`


connectOp
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' chrepr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromValueInChannel dinB chrepr
    => ToValueInChannel chrepr dinB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Node fA stateA isA osA chrepr mp /\ Id.Outlet oA
    -> Node fB stateB isB osB chrepr mp /\ Id.Inlet iB
    -> m (Link fA fB oA iB)
connectOp (nodeA /\ outletA) (nodeB /\ inletB) =
    connect outletA inletB nodeA nodeB


connectBySameRepr
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' chrepr mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => HasFallback chrepr
    => FromValueInChannel doutA chrepr
    => ToValueInChannel chrepr dinB
    => Proxy chrepr -- FIXME: Proxy is not needed anymore
    -> Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA chrepr mp
    -> Node fB stateB isB osB chrepr mp
    -> m (Link fA fB oA iB)
connectBySameRepr _ outletA inletB nodeA nodeB =
    -- FIXME: we always use `ChRepr.accept` here...
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) ViC.accept nodeA nodeB <#> Link.fromRaw


connectByDistinctRepr
    :: forall m fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' chreprA chreprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => ToValueInChannel chreprA doutA
    => FromValueInChannel dinB chreprB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Id.Outlet oA
    -> Id.Inlet iB
    -> (doutA -> ValueInChannel dinB)
    -> Node fA stateA isA osA chreprA mp
    -> Node fB stateB isB osB chreprB mp
    -> m (Link fA fB oA iB)
connectByDistinctRepr outletA inletB convertF nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) (ViC.inbetweenB convertF) nodeA nodeB <#> Link.fromRaw



connectAlike
    :: forall m fA fB oA iB d stateA stateB isA isB isB' osA osB osA' chreprA chreprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => ToValueInChannel chreprA d
    => FromValueInChannel d chreprB
    => HasOutlet osA osA' oA d
    => HasInlet isB isB' iB d
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA chreprA mp
    -> Node fB stateB isB osB chreprB mp
    -> m (Link fA fB oA iB)
connectAlike outletA inletB nodeA nodeB =
    unsafeConnect (Id.outletR outletA) (Id.inletR inletB) (ViC.inbetween' (Proxy :: _ d)) nodeA nodeB <#> Link.fromRaw


unsafeConnect
    :: forall m fA fB stateA stateB isA isB osA osB chreprA chreprB mp
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => Id.OutletR
    -> Id.InletR
    -> (chreprA -> ValueInChannel chreprB)
    -> Node fA stateA isA osA chreprA mp
    -> Node fB stateB isB osB chreprB mp
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
            -- FIXME: no value check is performed here
            sendToBIfFlagIsOn :: ValueInChannel chreprB -> m Unit
            sendToBIfFlagIsOn reprB = do -- TODO: Monad.whenM
                flagOn <- liftEffect $ Ref.read flagRef
                if flagOn then do
                  unsafeSendIn inletB reprB nodeB
                --   run nodeB
                else pure unit
        SignalX.runSignal $ subscribeOutletR outletA nodeA ~> (=<<) convertRepr ~> sendToBIfFlagIsOn
        (vicReprA :: ValueInChannel chreprA) <- atOutletR outletA nodeA
        sendToBIfFlagIsOn $ convertRepr =<< vicReprA
        pure $ RawLink.make nodeAId outletA inletB nodeBId $ Ref.write false flagRef


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' chreprA chreprB mo mi
     . MonadEffect mo
    => IsSymbol fA
    => IsSymbol fB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => Link fA fB oA iB
    -> Node fA stateA isA osA chreprA mi
    -> Node fB stateB isB osB chreprB mi
    -> mo Boolean
disconnect link (Node nodeAId _ _ _ _) (Node nodeBId _ _ _ _) =
    if (Link.fromNode link == nodeAId) && (Link.toNode link == nodeBId) then
        liftEffect (Link.cancel link) >>= (const $ pure true)
    else pure false



{- Private accessors -}


_getProtocol :: forall f state is os chrepr m. Node f state is os chrepr m -> Protocol state is os chrepr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall f state is os chrepr m. Node f state is os chrepr m -> Tracker state is os chrepr
_getTracker (Node _ _ tracker _ _) = tracker


{- Utils -}


logUpdates :: forall f state is os chrepr m. Show state => Show chrepr => Node f state is os chrepr m -> Signal String
logUpdates (Node _ _ tracker _ _) = show <$> tracker.all



{- Rawify -}


toRaw :: forall f state is os chrepr m. Node f state is os chrepr m -> Raw.Node state chrepr m
toRaw (Node nodeR shape tracker protocol fn) =
    Raw.Node
        nodeR
        shape
        tracker
        protocol
        $ Fn.toRaw fn