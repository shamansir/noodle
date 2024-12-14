module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Text.Output.Blessed (singleLine) as T
import Data.Map (lookup) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))

import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))

import Blessed.Internal.NodeKey (toRaw) as NodeKey
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element


import Cli.Bounds (collect, inletPos) as Bounds
import Cli.Keys (InfoBoxKey, InletButtonKey, NodeBoxKey, mainScreen)
import Cli.Keys (patchBox) as Key
import Cli.State (State)
import Cli.State (patch, replacePatch) as State
import Cli.Style (inletsOutlets) as Style

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.Link (LinkState)
import Cli.Components.Link (create, remove, store, append, runB) as CLink
import Cli.Components.SidePanel.Console as CC

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Wiring (class Wiring)
import Noodle.Patch (findRawNode, findRawLink, disconnectRaw, connectRaw) as Patch
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Link (id) as RawLink
import Noodle.Network as Network


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs strepr chrepr m
     . Wiring m
    => HasFallback chrepr
    => T.At T.StatusLine chrepr
    => T.At T.ChannelLabel chrepr
    => Id.PatchR
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Id.FamilyR -> Id.NodeR -> Id.InletR
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
        , Core.on Button.Press
            $ onPress patchR nodeBoxKey inletIdx nodeR inletR -- REM Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver familyR nodeR nodeBoxKey infoBoxKey inletIdx inletR mbRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey inletIdx
        ]
        []


onMouseOver
    :: forall tk pstate fs strepr chrepr m
     . T.At T.StatusLine chrepr
    => Id.FamilyR
    -> Id.NodeR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Id.InletR
    -> Maybe chrepr
    -> Signal chrepr
    -> _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
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
    mainScreen >~ Screen.render


onMouseOut :: forall tk pstate fs strepr chrepr m. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    -- REM FI.clear
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> pure unit -- REM II.hide
    mainScreen >~ Screen.render


onPress
    :: forall tk pstate fs strepr chrepr m
     . Wiring m
    => HasFallback chrepr
    => Id.PatchR
    -> NodeBoxKey
    -> Int
    -> Id.NodeR
    -> Id.InletR
    -> _
    -> _
    -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onPress patchR nodeTrgBoxKey inletIdx nodeTrgR inletTrgR _ _ = do
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
                                            CC.log "disconnect previous"
                                            nextPatch /\ success <- liftEffect $ Patch.disconnectRaw rawLink curPatch
                                            CLink.runB $ Key.patchBox >~ CLink.remove prevLinkState
                                            pure $ nextPatch /\ success
                                        Nothing -> pure (curPatch /\ false)

                            Nothing -> pure (curPatch /\ false)

                    case Patch.findRawNode nodeSrcR nextPatch /\ Patch.findRawNode nodeTrgR nextPatch of
                        Just rawNodeSrc /\ Just rawNodeTrg -> do
                            CC.log "both nodes were found"
                            nextPatch' /\ rawLink <- liftEffect $ Patch.connectRaw outletSrcR inletTrgR rawNodeSrc rawNodeTrg nextPatch

                            case RawLink.id rawLink of
                                Just rawLinkId -> do
                                    CC.log $ "RawLink ID is set: " <> show rawLinkId
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

                                    CLink.runB $ Key.patchBox >~ CLink.append linkState

                                    State.modify_ $ State.replacePatch patchR nextPatch'

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

                    linkCmp # Link.on Element.Click (onLinkClick holdsLink)

                    OI.hide
                    -}

                    pure unit
                else pure unit
            _ -> do
                pure unit
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

        CC.log "render screen"
        mainScreen >~ Screen.render -- FIXME: only re-render patchBox
