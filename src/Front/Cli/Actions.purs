module Front.Cli.Actions where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State (get, modify_) as State
import Control.Monad.Extra (whenJust)

import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (head) as Array
import Data.Map (lookup) as Map
import Data.Newtype (unwrap) as NT

import Blessed ((>~))
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp (lift, lift', runOn, runOnUnit, runOver, runM, runM', runOver, runOver', runEffect, getStateRef) as Blessed
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Node.Method (detach) as BNode

import Noodle.Wiring (class Wiring)
import Noodle.Id (NodeR, OutletR, InletR, PatchR, familyOf) as Id
import Noodle.Patch (Patch)
import Noodle.Patch (connectRaw, id, linksMap, disconnectRaw, findRawLink, disconnectAllFromTo, removeNode) as Patch
import Noodle.Patch.Links as Links
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, sendIn, shape, atInlet) as RawNode
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id) as RawLink
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (toFallback) as ViC
import Noodle.Repr.Tagged (class ValueTagged, valueTag, ValuePath(..)) as VT

import Cli.Keys (patchBox, mainScreen, NodeBoxKey) as Key
import Cli.Bounds (collect, inletPos) as Bounds
import Cli.State (State)
import Cli.State (patch, replacePatch, inletEditorCreated, markInletEditorCreated) as CState
import Cli.Components.Link (create, remove, store, append, on, forget, findTo) as CLink
import Cli.Components.Link (LinkCmpState)
import Cli.Components.ValueEditor (ValueEditorComp)
import Cli.Components.Link as CLink
import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel.Console as CC
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Tree as TP


type LinkStart strepr chrepr mi =
    { key :: Key.NodeBoxKey
    , node :: Raw.Node strepr chrepr mi
    , outlet :: Id.OutletR
    , outletIndex :: Int
    }


type LinkEnd strepr chrepr mi =
    { key :: Key.NodeBoxKey
    , node :: Raw.Node strepr chrepr mi
    , inlet :: Id.InletR
    , inletIndex :: Int
    }


data TrackCommand
    = Track
    | DontTrack


doTrack :: TrackCommand -> Boolean
doTrack Track = true
doTrack DontTrack = false


connect
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => VT.ValueTagged chrepr
    => TrackCommand
    -> LinkStart strepr chrepr mi
    -> LinkEnd strepr chrepr mi
    -> Patch pstate fs strepr chrepr mi
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
connect trackCmd source target inPatch = do
    state <- State.get

    let patchR = Patch.id inPatch

    nextPatch' /\ rawLink <- liftEffect $ Patch.connectRaw source.outlet target.inlet source.node target.node inPatch

    -- CC.log $ "RawLink ID is set: " <> show rawLinkId
    (linkState :: LinkCmpState Unit)
        <- CLink.create
            (RawLink.id rawLink)
            { node : source.key, outletIndex : source.outletIndex }
            { node : target.key, inletIndex  : target.inletIndex  }
            state.lastLink

    State.modify_ $ \s ->
        let
            nextLinks = CLink.store linkState s.links
        in
            s
                { links = nextLinks
                , lastLink = Just linkState
                }

    Blessed.runOnUnit $ Key.patchBox >~ CLink.append linkState

    State.modify_ $ CState.replacePatch patchR nextPatch'

    stateRef <- Blessed.getStateRef

    Blessed.runOnUnit $ CLink.on Element.Click
        (\lstate -> const <<< Blessed.lift <<< Blessed.runM' stateRef <<< disconnect trackCmd patchR rawLink lstate)
        linkState

    -- State.put nextState'

    -- Blessed.runOnUnit $ CLink.on Element.Click (\lstate -> const <<< Blessed.runOn nextState <<< onLinkClick patchR rawLink lstate) linkState

    when (doTrack trackCmd) $ CL.trackCommand $ QOp.connect rawLink
    SidePanel.refresh $ TP.sidePanel

    -- Blessed.runOnUnit $ CLink.on Element.Click (onLinkClick patchR rawLink) linkState
    State.modify_ $ _ { blockInletEditor = true }


disconnectLastAtInlet
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => TrackCommand
    -> Raw.Node strepr chrepr mi
    -> Id.InletR
    -> Patch pstate fs strepr chrepr mi
    -> BlessedOpM (State _ tk pstate fs strepr chrepr mi) mo (Patch pstate fs strepr chrepr mi /\ Boolean)
disconnectLastAtInlet trackCmd rawNode inletR inPatch = do
    state <- State.get
    let
        (mbPrevLink :: Maybe (LinkCmpState Unit)) =
            -- CLink.findTo (RawNode.id rawNode) inletR state.links
            Patch.linksMap inPatch
                # Links.findTo (RawNode.id rawNode) inletR
                # Array.head
                >>= \link -> Map.lookup (RawLink.id link) state.links
    case mbPrevLink of
        Just prevLinkState ->
            let
                prevLinkId = _.linkId $ NT.unwrap prevLinkState
            in
                case inPatch # Patch.findRawLink prevLinkId of
                    Just rawLink -> do
                        -- CC.log "disconnect previous"
                        nextPatch /\ success <- liftEffect $ Patch.disconnectRaw rawLink inPatch
                        Blessed.runOnUnit $ Key.patchBox >~ CLink.remove prevLinkState
                        when (doTrack trackCmd) $ CL.trackCommand $ QOp.disconnect rawLink
                        SidePanel.refresh $ TP.sidePanel
                        pure $ nextPatch /\ success
                    Nothing -> pure (inPatch /\ false)

        Nothing -> pure (inPatch /\ false)


disconnect
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => TrackCommand
    -> Id.PatchR
    -> Raw.Link
    -> LinkCmpState Unit
    -> _ {- EventJson -}
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
disconnect trackCmd patchR rawLink linkState _ = do
    curState <- State.get
    whenJust (CState.patch patchR curState) \patch -> do
        (nextPatch /\ _) <- Blessed.lift' $ Patch.disconnectRaw rawLink patch
        State.modify_ $ CState.replacePatch patchR nextPatch
        State.modify_ \s ->
            let
                nextLinks = CLink.forget linkState s.links
            in s { links = nextLinks }
        Blessed.runOnUnit $ Key.patchBox >~ CLink.remove linkState
        when (doTrack trackCmd) $ CL.trackCommand $ QOp.disconnect rawLink
        SidePanel.refresh $ TP.sidePanel
        Key.mainScreen >~ Screen.render


tryCallingInletEditor
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => VT.ValueTagged chrepr
    => HasFallback chrepr
    => Key.NodeBoxKey
    -> Raw.Node strepr chrepr mi
    -> Id.InletR
    -> Int
    -> Maybe (ValueEditorComp chrepr Unit Effect)
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
tryCallingInletEditor nodeBoxKey rawNode inletR inletIdx mbValueEditorOp = do
    state <- State.get
    let
        nodeR = RawNode.id rawNode
        familyR = Id.familyOf nodeR
    when (not state.blockInletEditor && isNothing state.inletEditorOpenedFrom) $ do
        -- TODO: also don't call if there is at least one link incoming
        vicCurValue <- RawNode.atInlet inletR rawNode
        let
            curValue = ViC.toFallback vicCurValue
            curValueTag = VT.valueTag (VT.Inlet familyR inletR) curValue

        whenJust mbValueEditorOp \{ create, inject, transpose } -> do
            if not $ CState.inletEditorCreated curValueTag state then do
                CC.log "Exact editor wasn't created, call `create` on it"
                _ <- Blessed.runOnUnit $ Blessed.runEffect unit create
                State.modify_ $ CState.markInletEditorCreated curValueTag
            else do
                CC.log "Skip creating the editor"
            CC.log "Remember the source node & inlet of the opened editor"
            State.modify_ $ _ { inletEditorOpenedFrom = Just (rawNode /\ inletR) }
            CC.log "Send current value in the editor"
            _ <- Blessed.runOnUnit $ Blessed.runEffect unit $ inject $ ViC.toFallback vicCurValue -- $ create *> (inject $ ViC.toFallback vicCurValue)
            nodeBounds <- case Map.lookup nodeR state.nodesBounds of
                            Just bounds -> pure bounds
                            Nothing -> Bounds.collect nodeR nodeBoxKey
            let inletPos = Bounds.inletPos nodeBounds inletIdx
            CC.log "Transpose the editor"
            _ <- Blessed.runOnUnit $ Blessed.runEffect unit $ transpose { x : inletPos.x, y : inletPos.y }
            pure unit
    State.modify_ $ _ { blockInletEditor = false }


removeNode
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => TrackCommand
    -> Id.PatchR
    -> Id.NodeR
    -> Key.NodeBoxKey
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
removeNode trackCmd patchR nodeR nodeBoxKey = do
    state <- State.get
    whenJust (CState.patch patchR state) \currentPatch -> do
        nextCurrentPatch <- Blessed.lift' $ Patch.disconnectAllFromTo nodeR currentPatch
        let nextLinks /\ linksToRemove = CLink.forgetAllFromTo nodeR state.links
        State.modify_ (\s ->
            s
                # CState.replacePatch (Patch.id currentPatch) nextCurrentPatch
                # _ { links = nextLinks }
        )
        Blessed.runOnUnit $ CLink.removeAllOf Key.patchBox linksToRemove
        nodeBoxKey >~ BNode.detach
        State.modify_
            $ CState.replacePatch (Patch.id currentPatch)
            $ Patch.removeNode nodeR nextCurrentPatch
        when (doTrack trackCmd) $ CL.trackCommand $ QOp.removeNode nodeR
        SidePanel.refresh TP.sidePanel
        Key.mainScreen >~ Screen.render