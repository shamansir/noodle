module Front.Cli.Actions where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State (get, modify_) as State

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

import Noodle.Wiring (class Wiring)
import Noodle.Id (familyOf, OutletR, InletR, PatchR) as Id
import Noodle.Patch (Patch)
import Noodle.Patch (connectRaw, id, linksMap, disconnectRaw, findRawLink) as Patch
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
import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel.Console as CC
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Tree as TP

connect
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => VT.ValueTagged chrepr
    => Key.NodeBoxKey
    -> Raw.Node strepr chrepr mi
    -> Id.OutletR
    -> Int
    -> Int
    -> Id.InletR
    -> Raw.Node strepr chrepr mi
    -> Key.NodeBoxKey
    -> Patch pstate fs strepr chrepr mi
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
connect nodeSrcBoxKey rawNodeSrc outletSrcR outletIdx inletIdx inletTrgR rawNodeTrg nodeTrgBoxKey inPatch = do
    state <- State.get

    let patchR = Patch.id inPatch

    nextPatch' /\ rawLink <- liftEffect $ Patch.connectRaw outletSrcR inletTrgR rawNodeSrc rawNodeTrg inPatch

    -- CC.log $ "RawLink ID is set: " <> show rawLinkId
    (linkState :: LinkCmpState Unit)
        <- CLink.create
            (RawLink.id rawLink)
            { node : nodeSrcBoxKey, outletIndex : outletIdx }
            { node : nodeTrgBoxKey, inletIndex  : inletIdx  }
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
        (\lstate -> const <<< Blessed.lift <<< Blessed.runM' stateRef <<< disconnect patchR rawLink lstate)
        linkState

    -- State.put nextState'

    -- Blessed.runOnUnit $ CLink.on Element.Click (\lstate -> const <<< Blessed.runOn nextState <<< onLinkClick patchR rawLink lstate) linkState

    CL.trackCommand $ QOp.connect rawLink
    SidePanel.refresh $ TP.sidePanel

    -- Blessed.runOnUnit $ CLink.on Element.Click (onLinkClick patchR rawLink) linkState
    State.modify_ $ _ { blockInletEditor = true }


disconnectLastAtInlet
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => Raw.Node strepr chrepr mi
    -> Id.InletR
    -> Patch pstate fs strepr chrepr mi
    -> BlessedOpM (State _ tk pstate fs strepr chrepr mi) mo (Patch pstate fs strepr chrepr mi /\ Boolean)
disconnectLastAtInlet rawNode inletR inPatch = do
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
                        CL.trackCommand $ QOp.disconnect rawLink
                        SidePanel.refresh $ TP.sidePanel
                        pure $ nextPatch /\ success
                    Nothing -> pure (inPatch /\ false)

        Nothing -> pure (inPatch /\ false)


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
    if not state.blockInletEditor && isNothing state.inletEditorOpenedFrom then do
        -- CC.log "Value editor is not blocked and not opened, try to open if exact one will be found"
        -- TODO: also don't call if there is at least one link incoming
        let nodeR = RawNode.id rawNode
        vicCurValue <- RawNode.atInlet inletR rawNode
        let
            curValue = ViC.toFallback vicCurValue
            curValueTag = VT.valueTag (VT.Inlet familyR inletR) curValue

        case mbValueEditorOp of
            Just { create, inject, transpose } -> do
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
                nodeBounds <- case Map.lookup nodeR state.locations of
                                Just bounds -> pure bounds
                                Nothing -> Bounds.collect nodeR nodeBoxKey
                let inletPos = Bounds.inletPos nodeBounds inletIdx
                CC.log "Transpose the editor"
                _ <- Blessed.runOnUnit $ Blessed.runEffect unit $ transpose { x : inletPos.x, y : inletPos.y }
                pure unit
            Nothing ->
                pure unit
                -- CC.log "No matching editor was found, skip"
    else
        pure unit
        -- CC.log "Editor is either blocked or opened already, don't call it"
    --CC.log "And don't block opening editor anymore"
    State.modify_ $ _ { blockInletEditor = false }


disconnect :: forall tk pstate fs strepr chrepr mi mo. Wiring mo => Id.PatchR -> Raw.Link -> LinkCmpState Unit -> _ -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
disconnect patchR rawLink linkState _ = do
    curState <- State.get
    let mbPatch = CState.patch patchR curState
    case mbPatch of
        Just patch -> do
            (nextPatch /\ _) <- Blessed.lift' $ Patch.disconnectRaw rawLink patch
            State.modify_ $ CState.replacePatch patchR nextPatch
            State.modify_ \s ->
                let
                    nextLinks = CLink.forget linkState s.links
                in s { links = nextLinks }
            Blessed.runOnUnit $ Key.patchBox >~ CLink.remove linkState
            CL.trackCommand $ QOp.disconnect rawLink
            SidePanel.refresh $ TP.sidePanel
            Key.mainScreen >~ Screen.render
        Nothing -> pure unit