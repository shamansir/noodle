module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console

import Unsafe.Coerce (unsafeCoerce)

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..))
import Data.Text.Output.Blessed (singleLine) as T
import Data.Map (lookup) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))

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
import Blessed.Internal.BlessedOp (lift, lift', runOn, runOnUnit, runOver, runM) as Blessed
import Blessed.Internal.BlessedSubj (Line)
import Blessed.Internal.JsApi (EventJson)
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element


import Cli.Bounds (collect, inletPos) as Bounds
import Cli.Keys (InfoBoxKey, InletButtonKey, NodeBoxKey)
import Cli.Keys (patchBox, mainScreen) as Key
import Cli.State (State)
import Cli.State (patch, replacePatch) as State
import Cli.Style (inletsOutlets) as Style
import Cli.Class.CliRenderer (class CliEditor, editorFor)

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.Link (LinkState)
import Cli.Components.Link (create, remove, store, append, on, forget) as CLink
import Cli.Components.SidePanel.Console as CC
import Cli.Components.SidePanel.CommandLog as CL


import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Wiring (class Wiring)
import Noodle.Repr.ChRepr (unwrap, ensureTo) as Repr
import Noodle.Patch (findRawNode, findRawLink, disconnectRaw, connectRaw) as Patch
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id) as RawLink
import Noodle.Network as Network
import Noodle.Text.NdfFile.Command.Quick as QOp

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T

import Cli.Components.ValueEditor (ValueEditor)


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs strepr chrepr m
     . HasFallback chrepr
    => T.At T.StatusLine chrepr
    => T.At T.ChannelLabel chrepr
    => Wiring m
    => CliEditor tk chrepr
    => Id.PatchR
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Id.FamilyR -> Id.NodeR -> Id.InletR -- TODO: we have `FamilyR` inside `NodeR`
    -> Int
    -> Maybe chrepr
    -> Signal chrepr
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs strepr chrepr m)
component patchR buttonKey nodeBoxKey infoBoxKey familyR nodeR inletR inletIdx mbRepr reprSignal =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.inlet inletIdx inletR mbRepr
        , Box.top $ Offset.px 0
        , Box.left $ left inletIdx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Element.Click -- Button.Press
            $ onPress patchR nodeBoxKey inletIdx familyR nodeR inletR mbRepr-- REM Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver familyR nodeR nodeBoxKey infoBoxKey inletIdx inletR mbRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey inletIdx
        ]
        []


onMouseOver
    :: forall tk pstate fs strepr chrepr mi mo
     . T.At T.StatusLine chrepr
    => Wiring mo
    => Id.FamilyR
    -> Id.NodeR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Id.InletR
    -> Maybe chrepr
    -> Signal chrepr
    -> _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onMouseOver familyR nodeIdR nodeBox infoBox idx inletR mbRepr reprSignal _ _ = do
    state <- State.get
    nodeBounds <- Bounds.collect nodeIdR nodeBox -- FIXME: load from state.locations
    let inletPos = Bounds.inletPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    infoBox >~ IB.inletInfo inletR
    SL.inletStatus familyR idx inletR mbRepr
    -- REM FI.inletStatus family idx inletId maybeRepr
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> do
            pure unit
            -- REM II.move { x : inletPos.x, y : inletPos.y - 1 }
            -- REM II.updateStatus II.Hover
    Key.mainScreen >~ Screen.render


onMouseOut :: forall tk pstate fs strepr chrepr mi mo. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    -- REM FI.clear
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> pure unit -- REM II.hide
    Key.mainScreen >~ Screen.render


onPress
    :: forall tk pstate fs strepr chrepr mi mo
     . Wiring mo
    => HasFallback chrepr
    => CliEditor tk chrepr
    => Id.PatchR
    -> NodeBoxKey
    -> Int
    -> Id.FamilyR
    -> Id.NodeR
    -> Id.InletR
    -> Maybe chrepr
    -> _
    -> _
    -> BlessedOp (State tk pstate fs strepr chrepr mi) mo
onPress patchR nodeTrgBoxKey inletIdx familyTrgR nodeTrgR inletTrgR mbRepr _ _ = do
        state <- State.get
        -- FIXME: load current patch from the state
        case state.lastClickedOutlet /\ State.patch patchR state of
            Just lco /\ Just curPatch ->
                if nodeTrgBoxKey /= lco.nodeKey then do

                    CC.log "inlet press"

                    let
                        (mbPrevLink :: Maybe (LinkState Unit)) =
                            Map.lookup (NodeKey.toRaw nodeTrgBoxKey) state.linksTo
                            >>= Map.lookup (Id.InletIndex inletIdx)
                        outletSrcR = lco.outletId
                        nodeSrcR = lco.nodeId
                        nodeSrcBoxKey = lco.nodeKey
                        outletIdx = lco.index


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
                                            pure $ nextPatch /\ success
                                        Nothing -> pure (curPatch /\ false)

                            Nothing -> pure (curPatch /\ false)

                    case Patch.findRawNode nodeSrcR nextPatch /\ Patch.findRawNode nodeTrgR nextPatch of
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

                                    -- Blessed.runOnUnit $ CLink.on Element.Click (onLinkClick patchR rawLink) linkState
                                    State.modify_ $ _ { blockInletEditor = true }

                                    pure unit
                                Nothing -> pure unit
                        _ -> pure unit


                    {- REM

                    let onodeId = Id.withNodeId lco.nodeId reflect'

                    logNdfCommandM $ Cmd.Connect (C.nodeId onodeId) (C.outputIndex lco.index) (C.nodeId $ reflect' inodeId) (C.inputIndex idx) -- TODO: log somewhere else in a special place
                    state' <- State.get
                    Key.commandLogBox >~ Box.setContent $ T.singleLine $ NdfFile.toTaggedNdfCode state'.commandLog

                    State.modify_  $ \s -> s
                        { network = wrapN $ Network.withPatch curPatchId (const nextPatch') $ unwrapN $ s.network
                        , linkWasMadeHack = true
                        }
                    -}

                    {- REM
                    OI.hide
                    -}

                    pure unit
                else pure unit
            _ -> do
                if not state.blockInletEditor && not state.inletEditorIsOpen then do
                    CC.log "Call editor"
                    -- TODO: also don't call if there is at least one link incoming

                    let (sendF :: chrepr -> Effect Unit) = const $ pure unit
                    let (curValue :: chrepr) = Repr.unwrap $ Repr.ensureTo mbRepr
                    let (mbValueEditor   :: Maybe (ValueEditor chrepr Unit Effect)) = editorFor (Proxy :: _ tk) familyTrgR nodeTrgBoxKey nodeTrgR inletTrgR mbRepr
                    let (mbValueEditorOp :: Maybe (_ /\ BlessedOp Unit Effect)) = (\f -> f curValue sendF) <$> mbValueEditor
                    case mbValueEditorOp of
                        Just (editorKey /\ editorOp) -> do
                            CC.log "Call exact editor"
                            _ <- Blessed.runOnUnit $ _blessedHelper unit editorOp
                            -- _ <- ((Blessed.runOver (Repr.unwrap $ Repr.ensureTo mbRepr) $ editorOp) :: BlessedOp' (State tk pstate fs strepr chrepr mi) mo _)
                            --   _ <- ((Blessed.runOver (Repr.unwrap $ Repr.ensureTo mbRepr) $ editorOp) :: BlessedOp' (State tk pstate fs strepr chrepr mi) mo _)

                            pure unit
                        Nothing -> pure unit

                    State.modify_ $ _ { blockInletEditor = false, inletEditorIsOpen = true }
                else
                    CC.log "Editor was blocked"
                State.modify_ $ _ { blockInletEditor = false }
                {- REM
                case mbEditorId of
                    Just editorId ->
                        -- FIXME: press handler triggers twice
                        if not state.linkWasMadeHack then do
                            let editor = Key.numValueEditor
                            inodeBounds <- Bounds.collect (Id.nodeIdR inodeId) inodeKey -- FIXME: use state.locations
                            State.modify_
                                (\s -> s
                                    { editors =
                                        Map.insert
                                            editorId
                                            (Just $ \wr ->
                                                case fromRepr $ wrap wr of
                                                    Just val -> Node.sendIn inode inputId val
                                                    Nothing -> pure unit
                                            )
                                            s.editors
                                    }
                                )
                            editor >~ Element.setTop $ Offset.px $ inodeBounds.top - 1
                            editor >~ Element.setLeft $ Offset.px $ inodeBounds.left
                            editor >~ TextArea.setValue ""
                            editor >~ Element.setFront
                            editor >~ Element.show
                            State.modify_  (_ { linkWasMadeHack = false })
                        else
                            State.modify_  (_ { linkWasMadeHack = false })
                    Nothing ->
                        pure unit
                -}

        State.modify_
            (_ { lastClickedOutlet = Nothing })

        -- CC.log "render screen"
        Key.mainScreen >~ Screen.render -- FIXME: only re-render patchBox


_blessedHelper :: forall s m a. MonadRec m => MonadEffect m => s -> BlessedOpM s Effect a -> BlessedOpM s m a
_blessedHelper s = Blessed.lift' <<< liftEffect <<< Blessed.runM s


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
            Key.mainScreen >~ Screen.render
        Nothing -> pure unit
