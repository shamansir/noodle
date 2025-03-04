module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console
import Effect.Ref (Ref)
import Effect.Ref (read, modify_) as Ref

import Unsafe.Coerce (unsafeCoerce)

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.Text.Output.Blessed (singleLine) as T
import Data.Map (lookup) as Map
import Data.Newtype (unwrap)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Type.Data.Symbol (class IsSymbol)

import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))

import Blessed.Internal.NodeKey (toRaw) as NodeKey
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOp', BlessedOpM)
import Blessed.Internal.BlessedOp (lift, lift', runOn, runOnUnit, runOver, runM, runM', runEffect, getStateRef) as Blessed
import Blessed.Internal.BlessedSubj (Line)
import Blessed.Internal.JsApi (EventJson)
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Element.Method (show, setFront, focus) as Element
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element
import Blessed.UI.Forms.TextArea.Method (setValue) as TextArea
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box

import Cli.Bounds (collect, inletPos) as Bounds
import Cli.Keys (InfoBoxKey, InletButtonKey, NodeBoxKey, ValueEditorKey)
import Cli.Keys (patchBox, mainScreen) as Key
import Cli.State (State)
import Cli.State (patch, replacePatch) as State
import Cli.Style (inletsOutlets) as Style
import Cli.Class.CliRenderer (class CliEditor, editorFor)
import Cli.State (Focus(..)) as Focus

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.Link (LinkState)
import Cli.Components.Link (create, remove, store, append, on, forget) as CLink
import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel.Console as CC
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Tree as TP
import Cli.Components.Editor.Textual (tveKey) as VEditor
import Cli.Components.NodeBox.InletIndicator as II
import Cli.Components.NodeBox.OutletIndicator as OI
import Cli.Components.ValueEditor (ValueEditor, ValueEditorComp)

import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Wiring (class Wiring)
import Noodle.Patch (findRawNode, findRawLink, disconnectRaw, connectRaw) as Patch
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, sendIn, shape) as RawNode
import Noodle.Network as Network
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ValueEncode, encodeValue) as Ndf
import Noodle.Text.NdfFile.Types (EncodedValue(..)) as Ndf
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (toFallback) as ViC
import Noodle.Repr.Tagged (class Tagged) as CT
import Noodle.Toolkit (ToolkitKey)
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Fn.Shape.Temperament (default) as Temperament
import Noodle.Raw.Fn.Shape (temperamentOf) as RawShape

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T



--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall (tk :: ToolkitKey) (vekey :: Symbol) pstate fs strepr chrepr m
     . T.At T.StatusLine chrepr
    => T.At T.ChannelLabel chrepr
    => Ndf.ValueEncode chrepr
    => HasFallback chrepr
    => CT.Tagged chrepr
    => Wiring m
    => CliEditor tk chrepr
    => Ref (State tk pstate fs strepr chrepr m)
    -> Id.PatchR
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Raw.Node strepr chrepr m
    -> Id.InletR -> Int
    -> ValueInChannel chrepr
    -> Signal (ValueInChannel chrepr)
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs strepr chrepr m)
component stateRef patchR buttonKey nodeBoxKey infoBoxKey rawNode inletR inletIdx vicRepr reprSignal =
    let
        nodeR = RawNode.id rawNode
        familyR = Id.familyOf nodeR
        temper = fromMaybe Temperament.default $ RawShape.temperamentOf inletR $ RawNode.shape rawNode
        (sendF :: chrepr -> Effect Unit) =
            \reprV -> do
                state <- Ref.read stateRef
                case state.inletEditorOpenedFrom of
                    Just (editorRawNode /\ editorInletR) -> do
                        -- Blessed.runM' stateRef $ CC.log "Editor has submitted a value, clear opened source and send value to inlet"
                        stateRef # Ref.modify_ (_ { inletEditorOpenedFrom = Nothing }) -- FIXME: could the editor component execute onSubmit in `BlessedOp`?
                        RawNode.sendIn editorInletR reprV editorRawNode
                        Blessed.runM' stateRef $ CL.trackCommand $ QOp.sendIn nodeR inletR $ fromMaybe (Ndf.EncodedValue "?") $ Ndf.encodeValue reprV
                    Nothing ->
                        pure unit
        (mbValueEditor :: Maybe (ValueEditor chrepr Unit Effect)) = editorFor (Proxy :: _ tk) familyR nodeBoxKey nodeR inletR vicRepr
        (mbValueEditorOp :: Maybe (ValueEditorComp Unit Effect)) = (\f -> f (ViC.toFallback vicRepr) sendF) <$> mbValueEditor
    in B.button buttonKey
        [ Box.content $ T.singleLine $ T.inlet inletIdx inletR vicRepr
        , Box.top $ Offset.px 0
        , Box.left $ left inletIdx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Element.Click -- Button.Press
            $ onPress mbValueEditorOp patchR nodeBoxKey inletIdx rawNode inletR vicRepr-- REM Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver patchR (RawNode.id rawNode) inletR nodeBoxKey infoBoxKey inletIdx temper vicRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey inletIdx
        ]
        []


onMouseOver
    :: forall tk pstate fs strepr chrepr mi mo
     . T.At T.StatusLine chrepr
    => Wiring mo
    => Id.PatchR
    -> Id.NodeR
    -> Id.InletR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Temperament
    -> ValueInChannel chrepr
    -> Signal (ValueInChannel chrepr)
    -> _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onMouseOver patchR nodeR inletR nodeBox infoBox idx temper vicRepr reprSignal _ _ = do
    State.modify_ $ _ { mouseOverFocus = Just $ Focus.Inlet patchR nodeR inletR }
    nodeBounds <- Bounds.collect nodeR nodeBox
    -- nodeBounds <- case Map.lookup nodeR state.locations of
    --                     Just bounds -> pure bounds
    --                     Nothing -> Bounds.collect nodeR nodeBox
    let inletPos = Bounds.inletPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    infoBox >~ IB.inletInfo inletR
    SL.inletStatus (Id.familyOf nodeR) idx inletR vicRepr
    -- REM FI.inletStatus family idx inletId maybeRepr
    II.move { x : inletPos.x, y : inletPos.y }
    II.updateStatus $ II.Hover temper
    Key.mainScreen >~ Screen.render


onMouseOut :: forall tk pstate fs strepr chrepr mi mo. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onMouseOut infoBox idx _ _ = do
    State.modify_ $ _ { mouseOverFocus = Nothing }
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    -- REM FI.clear
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> II.hide
    Key.mainScreen >~ Screen.render


onPress
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    -- => HasFallback chrepr
    => CT.Tagged chrepr
    => CliEditor tk chrepr
    => Maybe (ValueEditorComp Unit Effect)
    -> Id.PatchR
    -> NodeBoxKey
    -> Int
    -> Raw.Node strepr chrepr mi
    -> Id.InletR
    -> ValueInChannel chrepr
    -> _
    -> _
    -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onPress mbValueEditorOp patchR nodeBoxKey inletIdx rawNode inletR vicRepr _ _ = do
        state <- State.get
        -- FIXME: load current patch from the state
        case state.lastClickedOutlet /\ State.patch patchR state of
            Just lco /\ Just curPatch ->
                if nodeBoxKey /= lco.nodeKey then do

                    -- CC.log "inlet press"
                    OI.hide

                    let
                        (mbPrevLink :: Maybe (LinkState Unit)) =
                            Map.lookup (NodeKey.toRaw nodeBoxKey) state.linksTo
                            >>= Map.lookup (Id.InletIndex inletIdx)
                        outletSrcR = lco.outletId
                        nodeSrcR = lco.nodeId
                        nodeSrcBoxKey = lco.nodeKey
                        outletIdx = lco.index
                        inletTrgR = inletR
                        nodeTrgR = RawNode.id rawNode
                        nodeTrgBoxKey = nodeBoxKey

                    nextPatch /\ isDisconnected <-
                        case mbPrevLink of
                            Just prevLinkState ->
                                let
                                    prevLinkId = _.inPatch $ unwrap prevLinkState
                                in
                                    case curPatch # Patch.findRawLink prevLinkId of
                                        Just rawLink -> do
                                            -- CC.log "disconnect previous"
                                            nextPatch /\ success <- liftEffect $ Patch.disconnectRaw rawLink curPatch
                                            Blessed.runOnUnit $ Key.patchBox >~ CLink.remove prevLinkState
                                            CL.trackCommand $ QOp.disconnect rawLink
                                            SidePanel.refresh $ TP.sidePanel
                                            pure $ nextPatch /\ success
                                        Nothing -> pure (curPatch /\ false)

                            Nothing -> pure (curPatch /\ false)

                    case Patch.findRawNode nodeSrcR nextPatch /\ Patch.findRawNode nodeTrgR nextPatch of -- FIXME: we already have target node here, no need to search for it
                        Just rawNodeSrc /\ Just rawNodeTrg -> do
                            -- CC.log "both nodes were found"
                            nextPatch' /\ rawLink <- liftEffect $ Patch.connectRaw outletSrcR inletTrgR rawNodeSrc rawNodeTrg nextPatch

                            case RawLink.id rawLink of
                                Just rawLinkId -> do
                                    -- CC.log $ "RawLink ID is set: " <> show rawLinkId
                                    (linkState :: LinkState Unit)
                                        <- CLink.create
                                                rawLinkId
                                                { id : nodeSrcR, key : nodeSrcBoxKey }
                                                (Id.OutletIndex outletIdx)
                                                { id : nodeTrgR, key : nodeTrgBoxKey  }
                                                (Id.InletIndex inletIdx)
                                                state.lastLink

                                    State.modify_ $ \s ->
                                        let
                                            nextLinksFrom /\ nextLinksTo = CLink.store linkState $ s.linksFrom /\ s.linksTo
                                        in
                                            s
                                                { linksFrom = nextLinksFrom
                                                , linksTo = nextLinksTo
                                                , lastLink = Just linkState
                                                }

                                    Blessed.runOnUnit $ Key.patchBox >~ CLink.append linkState

                                    State.modify_ $ State.replacePatch patchR nextPatch'

                                    Blessed.runOnUnit $ CLink.on Element.Click (\lstate -> const <<< Blessed.runOn state <<< onLinkClick patchR rawLink lstate) linkState

                                    CL.trackCommand $ QOp.connect rawLink
                                    SidePanel.refresh $ TP.sidePanel

                                    -- Blessed.runOnUnit $ CLink.on Element.Click (onLinkClick patchR rawLink) linkState
                                    State.modify_ $ _ { blockInletEditor = true }

                                    pure unit
                                Nothing -> pure unit
                        _ -> pure unit

                    {- REM
                    hide
                    -}

                    pure unit
                else pure unit
            _ -> do
                if not state.blockInletEditor && isNothing state.inletEditorOpenedFrom then do
                    -- CC.log "Value editor is not blocked and not opened, try to open if exact one will be found"
                    -- TODO: also don't call if there is at least one link incoming
                    let nodeR = RawNode.id rawNode

                    case mbValueEditorOp of
                        Just { create, transpose } -> do
                            -- CC.log "Exact editor was found, call it"
                            _ <- Blessed.runOnUnit $ Blessed.runEffect unit create
                            nodeBounds <- case Map.lookup nodeR state.locations of
                                            Just bounds -> pure bounds
                                            Nothing -> Bounds.collect nodeR nodeBoxKey
                            let inletPos = Bounds.inletPos nodeBounds inletIdx
                            _ <- Blessed.runOnUnit $ Blessed.runEffect unit $ transpose { x : inletPos.x, y : inletPos.y - 1 }
                            -- CC.log "Remember the source node & inlet of the opened editor"
                            State.modify_ $ _ { inletEditorOpenedFrom = Just (rawNode /\ inletR) }
                        Nothing ->
                            pure unit
                            -- CC.log "No matching editor was found, skip"
                else
                    pure unit
                    -- CC.log "Editor is either blocked or opened already, don't call it"
                --CC.log "And don't block opening editor anymore"
                State.modify_ $ _ { blockInletEditor = false }

        State.modify_
            (_ { lastClickedOutlet = Nothing })

        -- CC.log "render screen"
        Key.mainScreen >~ Screen.render -- FIXME: only re-render patchBox


onLinkClick :: forall id tk pstate fs strepr chrepr mi mo. Wiring mo => Id.PatchR -> Raw.Link -> LinkState Unit -> Line <^> id → {- EventJson → -} BlessedOp (State tk pstate fs strepr chrepr mi) mo
onLinkClick patchR rawLink linkState _ = do
    CC.log $ "Click link"
    curState <- State.get
    let mbPatch = State.patch patchR curState
    case mbPatch of
        Just patch -> do
            (nextPatch /\ _) <- Blessed.lift' $ Patch.disconnectRaw rawLink patch
            State.modify_ $ State.replacePatch patchR nextPatch
            State.modify_ \s ->
                let
                    nextLinksFrom /\ nextLinksTo = CLink.forget linkState (s.linksFrom /\ s.linksTo)
                in s { linksFrom = nextLinksFrom, linksTo = nextLinksTo }
            Blessed.runOnUnit $ Key.patchBox >~ CLink.remove linkState
            CL.trackCommand $ QOp.disconnect rawLink
            SidePanel.refresh $ TP.sidePanel
            Key.mainScreen >~ Screen.render
        Nothing -> pure unit
