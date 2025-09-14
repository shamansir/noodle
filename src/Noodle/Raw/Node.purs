module Noodle.Raw.Node where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write) as Ref

import Type.Proxy (Proxy(..))

import Control.Monad.Rec.Class (class MonadRec)

import Data.Map (Map)
import Data.Map (lookup, fromFoldable) as Map
import Data.Map.Extra (mapKeys) as Map
import Data.UniqueHash (generate) as UH
import Data.Maybe (fromMaybe)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (wrap) as NT
import Data.Functor.Extra ((<##>))

import Signal (Signal, (~>))
import Signal.Extra (runSignal) as SignalX

import Noodle.Wiring (class Wiring)
import Noodle.Id (NodeR, FamilyR, InletR, OutletR, family, familyOf, nodeR_, inletRName, outletRName, unsafeInletR, unsafeOutletR) as Id
import Noodle.Raw.Id (inletR, outletR) as Id
import Noodle.Fn.Generic.Updates (UpdateFocus(..), InletsUpdate(..)) as Fn
import Noodle.Fn.Generic.Updates (MergedUpdateRec, toRecord) as Updates
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.Tagged (class ValueTagged, valueTag, acceptByTag, ValuePath(..)) as VT
import Noodle.Repr.Tagged (inletN, outletN) as Path
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (_reportMissingKey, accept, decline) as ViC
import Noodle.Raw.Fn (Fn) as Raw
import Noodle.Raw.Fn (make, run', toReprableState) as RawFn
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Shape (Shape, ValueTag, failed, InletDefR, OutletDefR) as Raw
import Noodle.Raw.Fn.Shape (make, inlets, outlets, hasHotInlets, isHotInlet, indexOfInlet, indexOfOutlet, tagOfInlet, tagOfOutlet, inletsCount, outletsCount) as RawShape
import Noodle.Raw.Fn.Protocol (make, getInlets, getOutlets, getState, sendIn, sendOut, modifyState) as RawProtocol
import Noodle.Raw.Fn.Tracker (Tracker) as Raw
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Raw.Fn.Tracker (toReprableState) as RawTracker
import Noodle.Raw.Fn.Protocol (toReprableState) as RawProtocol
import Noodle.Raw.Fn.Updates (toSignature) as RawUpdates
import Noodle.Fn.Signature (Signature)
import Noodle.Fn.Signature (reorder, args, outs) as Sig
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (make, fromNode, toNode, cancel) as RawLink
import Noodle.Fn.Shape.Temperament as Temp


data Node state (chrepr :: Type) (m :: Type -> Type)
    = Node
        Id.NodeR
        Raw.Shape
        (Raw.Tracker state chrepr)
        (Raw.Protocol state chrepr)
        (Raw.Fn state chrepr m)


type InitialInletsValues  chrepr = Map Id.InletR  chrepr
type InitialOutletsValues chrepr = Map Id.OutletR chrepr


type InletsValues  chrepr = Map Id.InletR  (ValueInChannel chrepr)
type OutletsValues chrepr = Map Id.OutletR (ValueInChannel chrepr)


type OrderedInletsValues  chrepr = Map (Int /\ Id.InletR)  (ValueInChannel chrepr)
type OrderedOutletsValues chrepr = Map (Int /\ Id.OutletR) (ValueInChannel chrepr)


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
    -> InitialInletsValues  chrepr
    -> InitialOutletsValues chrepr
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
    -> InitialInletsValues  chrepr
    -> InitialOutletsValues chrepr
    -> Raw.Fn state chrepr mp
    -> m (Node state chrepr mp)
_makeWithFn family state rawShape inletsMap outletsMap fn = do
    uniqueHash <- liftEffect $ UH.generate
    let nodeId = Id.nodeR_ family uniqueHash
    tracker /\ protocol <- RawProtocol.make state inletsMap outletsMap
    pure $ Node nodeId rawShape tracker protocol fn



_fromSignature
    :: forall m state chrepr mp
     . MonadEffect m
    => VT.ValueTagged chrepr
    => Id.FamilyR
    -> state
    -> Signature chrepr chrepr
    -> Raw.Process state chrepr mp
    -> m (Node state chrepr mp)
_fromSignature family state sig =
    make family state
      (RawShape.make
        { inlets  : mapWithIndex inletDef  $ Sig.args sig
        , outlets : mapWithIndex outletDef $ Sig.outs sig
        }
      )
      (Map.fromFoldable $ lmap Id.unsafeInletR  <$> Sig.args sig)
      (Map.fromFoldable $ lmap Id.unsafeOutletR <$> Sig.outs sig)
    where
      inletDef  idx (inletS  /\ repr) =
        let inletR = Id.unsafeInletR inletS
        in { name : inletR, order : idx,  tag : VT.valueTag (VT.Inlet family inletR) repr, temp : Temp.Hot }
      outletDef idx (outletS /\ repr) =
        let outletR = Id.unsafeOutletR outletS
        in { name : outletR, order : idx, tag : VT.valueTag (VT.Outlet family outletR) repr }


{- Running -}


-- TODO: private
_runOnInletUpdates
    :: forall state chrepr m
    .  Wiring m
    => HasFallback chrepr
    => Node state chrepr m
    -> m Unit
_runOnInletUpdates node =
  SignalX.runSignal $ _subscribeInlets node ~> isHotUpdate ~> runOnlyIfHasHotInlet
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


inletsCount :: forall state chrepr m. Node state chrepr m -> Int
inletsCount = shape >>> RawShape.inletsCount


outletsCount :: forall state chrepr m. Node state chrepr m -> Int
outletsCount = shape >>> RawShape.outletsCount


inlets :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (InletsValues chrepr)
inlets node = liftEffect $ RawProtocol.getInlets $ _getProtocol node


outlets :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (OutletsValues chrepr)
outlets node = liftEffect $ RawProtocol.getOutlets $ _getProtocol node


state :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m state
state node = liftEffect $ RawProtocol.getState $ _getProtocol node


atInlet :: forall m state chrepr mp. MonadEffect m => Id.InletR -> Node state chrepr mp -> m (ValueInChannel chrepr)
atInlet inletR node = inlets node <#> Map.lookup inletR <#> (ViC._reportMissingKey $ Id.inletRName inletR)


atOutlet :: forall m state chrepr mp. MonadEffect m => Id.OutletR -> Node state chrepr mp -> m (ValueInChannel chrepr)
atOutlet outletR node = outlets node <#> Map.lookup outletR <#> (ViC._reportMissingKey $ Id.outletRName outletR)


curChanges :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> m (NodeChanges state chrepr)
curChanges node = do
  let nshape = shape node
  is <- inlets node
  os <- outlets node
  s  <- state node
  pure
    { focus : Fn.Everything
    , state : s
    , inlets : is # orderInlets nshape
    , outlets : os # orderOutlets nshape
    }


inlets' :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> Array (Raw.InletDefR /\ (m (ValueInChannel chrepr)))
inlets' node@(Node _ shape _ _ _) = (\def -> NT.wrap def /\ atInlet def.name node) <$> RawShape.inlets shape


outlets' :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> Array (Raw.OutletDefR /\ (m (ValueInChannel chrepr)))
outlets' node@(Node _ shape _ _ _) = (\def -> NT.wrap def /\ atOutlet def.name node) <$> RawShape.outlets shape


{- Private accessors -}


_getProtocol :: forall state chrepr m. Node state chrepr m -> Raw.Protocol state chrepr
_getProtocol (Node _ _ _ protocol _) = protocol


_getTracker :: forall state chrepr m. Node state chrepr m -> Raw.Tracker state chrepr
_getTracker (Node _ _ tracker _ _) = tracker


{- Subscriptions -}


type NodeChanges state chrepr = Updates.MergedUpdateRec state (OrderedInletsValues chrepr) (OrderedOutletsValues chrepr)


subscribeInlet :: forall state chrepr m. Id.InletR -> Node state chrepr m -> Signal (ValueInChannel chrepr)
subscribeInlet inletR (Node _ _ tracker _ _) = (ViC._reportMissingKey $ Id.inletRName inletR) <$> Map.lookup inletR <$> Tuple.snd <$> tracker.inlets


subscribeInlets :: forall state chrepr m. Node state chrepr m -> Signal (OrderedInletsValues chrepr)
subscribeInlets (Node _ shape tracker _ _) = Tuple.snd <$> tracker.inlets <#> orderInlets shape


_subscribeInlets :: forall state chrepr m. Node state chrepr m -> Signal (Fn.InletsUpdate /\ OrderedInletsValues chrepr)
_subscribeInlets (Node _ shape tracker _ _) = tracker.inlets <##> orderInlets shape


subscribeOutlet :: forall state chrepr m. Id.OutletR -> Node state chrepr m -> Signal (ValueInChannel chrepr)
subscribeOutlet outletR (Node _ shape tracker _ _) = (ViC._reportMissingKey $ Id.outletRName outletR) <$> Map.lookup outletR <$> Tuple.snd <$> tracker.outlets


subscribeOutlets :: forall state chrepr m. Node state chrepr m -> Signal (OrderedOutletsValues chrepr)
subscribeOutlets (Node _ shape tracker _ _) = Tuple.snd <$> tracker.outlets <#> orderOutlets shape


subscribeState :: forall state chrepr m. Node state chrepr m -> Signal state
subscribeState (Node _ _ tracker _ _) = tracker.state


subscribeChanges :: forall state chrepr m. Node state chrepr m -> Signal (NodeChanges state chrepr)
subscribeChanges (Node _ shape tracker _ _) =
  tracker.all
    <#> Updates.toRecord
    <#> \chs -> chs
      { inlets = orderInlets shape chs.inlets
      , outlets = orderOutlets shape chs.outlets
      }


subscribeChangesAsSignature :: forall state chrepr m. Node state chrepr m -> Signal (Signature (ValueInChannel chrepr) (ValueInChannel chrepr))
subscribeChangesAsSignature (Node nodeR shape tracker _ _) =
  tracker.all
    <#> RawUpdates.toSignature nodeR
    <#> Sig.reorder
      (Id.inletR >>> flip RawShape.indexOfInlet shape)
      (Id.outletR >>> flip RawShape.indexOfOutlet shape)



{- Send data -}


infixr 6 sendInOp as #->
infixr 6 sendOutOp as @->


sendIn :: forall m state chrepr mp. MonadEffect m => Id.InletR -> chrepr -> Node state chrepr mp -> m Unit
sendIn inletR repr = sendIn_ inletR $ ViC.accept repr


sendIn_ :: forall m state chrepr mp. MonadEffect m => Id.InletR -> ValueInChannel chrepr -> Node state chrepr mp -> m Unit
sendIn_ inletR vicRepr = liftEffect <<< RawProtocol.sendIn inletR vicRepr <<< _getProtocol


sendInOp :: forall m state chrepr mp. MonadEffect m => Node state chrepr mp -> Id.InletR /\ chrepr -> m Unit
sendInOp node (input /\ din) = sendIn input din node


sendOut :: forall m state chrepr mp. MonadEffect m => Id.OutletR -> chrepr -> Node state chrepr mp -> m Unit
sendOut outletR repr = sendOut_ outletR $ ViC.accept repr


sendOut_ :: forall m state chrepr mp. MonadEffect m => Id.OutletR -> ValueInChannel chrepr -> Node state chrepr mp -> m Unit
sendOut_ outletR vicRepr = liftEffect <<< RawProtocol.sendOut outletR vicRepr <<< _getProtocol


sendOutOp :: forall m state chrepr mp. MonadEffect m =>  Node state chrepr mp -> Id.OutletR /\ chrepr -> m Unit
sendOutOp node (output /\ dout) = sendOut output dout node


modifyState :: forall m state chrepr mp. MonadEffect m => (state -> state) -> Node state chrepr mp -> m Unit
modifyState f = liftEffect <<< RawProtocol.modifyState f <<< _getProtocol


setState :: forall m state chrepr mp. MonadEffect m => state -> Node state chrepr mp -> m Unit
setState = modifyState <<< const


{- Connecting -}


-- TODO: reuse those in `Noodle.Node` implementation instead of what is implemented there


connect
    :: forall m stateA stateB chreprA chreprB mp
     . Wiring m
    => VT.ValueTagged chreprB
    => Id.OutletR
    -> Id.InletR
    -> (chreprA -> chreprB)
    -> Node stateA chreprA mp
    -> Node stateB chreprB mp
    -> m Raw.Link
connect
    outletA
    inletB
    toReprB
    nodeA@(Node nodeAId nodeAShape _ _ _)
    nodeB@(Node nodeBId nodeBShape _ _ _) =
    do
        flagRef <- liftEffect $ Ref.new true
        let
            sendToBIfFlagIsOn :: ValueInChannel chreprB -> m Unit
            sendToBIfFlagIsOn reprB = do -- TODO: Monad.whenM
                flagOn <- liftEffect $ Ref.read flagRef
                if flagOn then do
                  sendIn_ inletB reprB nodeB
                --   run nodeB
                else pure unit
        (vicReprA :: ValueInChannel chreprA) <- atOutlet outletA nodeA

        SignalX.runSignal $ subscribeOutlet outletA nodeA ~> (=<<) convertRepr ~> sendToBIfFlagIsOn

        sendToBIfFlagIsOn $ convertRepr =<< vicReprA
        pure $ RawLink.make nodeAId outletA inletB nodeBId $ Ref.write false flagRef
    where
      (outletATag :: Raw.ValueTag) = RawShape.tagOfOutlet outletA nodeAShape # fromMaybe Raw.failed
      (inletBTag  :: Raw.ValueTag) = RawShape.tagOfInlet  inletB  nodeBShape # fromMaybe Raw.failed
      convertRepr :: chreprA -> ValueInChannel chreprB
      convertRepr reprA =
          if VT.acceptByTag (Proxy :: _ chreprB) { incoming : outletATag, current : inletBTag }
            then ViC.accept $ toReprB reprA
            else ViC.decline


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


 -- FIXME: Find a faster way to do it
orderInlets :: forall chrepr. Raw.Shape -> InletsValues chrepr -> OrderedInletsValues chrepr
orderInlets shape = Map.mapKeys toKey
  where
    toKey inletR = qiindex inletR /\ inletR
    qiindex inletR = fromMaybe maxN $ RawShape.indexOfInlet inletR shape
    maxN = Array.length $ RawShape.inlets shape


 -- FIXME: Find a faster way to do it
orderOutlets :: forall chrepr. Raw.Shape -> OutletsValues chrepr -> OrderedOutletsValues chrepr
orderOutlets shape = Map.mapKeys toKey
  where
    toKey outletR = qoindex outletR /\ outletR
    qoindex outletR = fromMaybe maxN $ RawShape.indexOfOutlet outletR shape
    maxN = Array.length $ RawShape.outlets shape


toReprableState :: forall state strepr chrepr m. HasFallback state => StRepr state strepr => Node state chrepr m -> Node strepr chrepr m
toReprableState (Node nodeR shape tracker protocol fn) =
    Node
        nodeR
        shape
        (RawTracker.toReprableState tracker)
        (RawProtocol.toReprableState protocol)
        $ RawFn.toReprableState fn
