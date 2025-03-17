module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console
import Effect.Ref (Ref)
import Effect.Ref (read, modify_) as Ref

import Unsafe.Coerce (unsafeCoerce)

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, modify, modify_, put) as State
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.Text.Output.Blessed (singleLine) as T
import Data.Map (lookup) as Map
import Data.Newtype (unwrap)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (head) as Array

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
import Blessed.Internal.BlessedOp (lift, lift', runOn, runOnUnit, runOver, runM, runM', runOver, runOver', runEffect, getStateRef) as Blessed
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
import Cli.State (patch, replacePatch, inletEditorCreated, markInletEditorCreated) as CState
import Cli.Style (inletsOutlets) as Style
import Cli.Class.CliRenderer (class CliEditor, editorFor)
import Cli.State (Focus(..)) as Focus

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.Link (LinkCmpState)
import Cli.Components.Link (create, remove, store, append, on, forget, findTo) as CLink
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
import Noodle.Patch (findRawNode, findRawLink, disconnectRaw, connectRaw, linksMap) as Patch
import Noodle.Patch.Links as Links
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, sendIn, shape, atInlet) as RawNode
import Noodle.Network as Network
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ValueEncode, encodeValue) as Ndf
import Noodle.Text.NdfFile.Types (EncodedValue(..)) as Ndf
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (toFallback) as ViC
import Noodle.Repr.Tagged (class ValueTagged, valueTag, ValuePath(..)) as VT
import Noodle.Toolkit (ToolkitKey)
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Fn.Shape.Temperament (default) as Temperament
import Noodle.Raw.Fn.Shape (temperamentOf) as RawShape

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T


import Front.Cli.Actions as Actions

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
    => VT.ValueTagged chrepr
    => Wiring m
    => CliEditor tk chrepr
    => Ref (State _ tk pstate fs strepr chrepr m)
    -> Id.PatchR
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Raw.Node strepr chrepr m
    -> Id.InletR -> Int
    -> ValueInChannel chrepr
    -> Signal (ValueInChannel chrepr)
    -- -> Raw.Node
    -> Core.Blessed (State _ tk pstate fs strepr chrepr m)
component stateRef patchR buttonKey nodeBoxKey infoBoxKey rawNode inletR inletIdx vicRepr reprSignal =
    let
        nodeR = RawNode.id rawNode
        familyR = Id.familyOf nodeR
        temper = fromMaybe Temperament.default $ RawShape.temperamentOf inletR $ RawNode.shape rawNode
        (mbValueEditor :: Maybe (ValueEditor chrepr Unit Effect)) = editorFor (Proxy :: _ tk) familyR nodeBoxKey nodeR inletR vicRepr
        (mbValueEditorOp :: Maybe (ValueEditorComp chrepr Unit Effect)) =
            (\f -> f (ViC.toFallback vicRepr) $ sendFromEditor stateRef nodeR inletR)
            <$> mbValueEditor
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
            $ onPress mbValueEditorOp familyR patchR nodeBoxKey inletIdx rawNode inletR-- REM Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver patchR (RawNode.id rawNode) inletR nodeBoxKey infoBoxKey inletIdx temper vicRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey inletIdx
        ]
        []


sendFromEditor :: forall tk pstate fs strepr chrepr m. Ndf.ValueEncode chrepr => Ref (State _ tk pstate fs strepr chrepr m) -> Id.NodeR -> Id.InletR -> chrepr -> Effect Unit
sendFromEditor stateRef nodeR inletR reprV = do
    state <- Ref.read stateRef
    case state.inletEditorOpenedFrom of
        Just (editorRawNode /\ editorInletR) -> do
            -- Blessed.runM' stateRef $ CC.log "Editor has submitted a value, clear opened source and send value to inlet"
            stateRef # Ref.modify_ (_ { inletEditorOpenedFrom = Nothing }) -- FIXME: could the editor component execute onSubmit in `BlessedOp`?
            RawNode.sendIn editorInletR reprV editorRawNode
            Blessed.runM' stateRef $ CL.trackCommand $ QOp.sendIn nodeR inletR $ fromMaybe (Ndf.EncodedValue "?") $ Ndf.encodeValue reprV
        Nothing ->
            pure unit


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
    -> _ -> _ -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
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


onMouseOut :: forall tk pstate fs strepr chrepr mi mo. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
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
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => CliEditor tk chrepr
    => Maybe (ValueEditorComp chrepr Unit Effect)
    -> Id.FamilyR
    -> Id.PatchR
    -> NodeBoxKey
    -> Int
    -> Raw.Node strepr chrepr mi
    -> Id.InletR
    -> _
    -> _
    -> BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
onPress mbValueEditorOp familyR patchR nodeBoxKey inletIndex rawNode inletR _ _ = do
        state <- State.get
        -- FIXME: load current patch from the state
        case state.lastClickedOutlet /\ CState.patch patchR state of
            Just lco /\ Just curPatch ->
                if nodeBoxKey /= lco.nodeKey then do

                    -- CC.log "inlet press"
                    OI.hide

                    let
                        (mbPrevLink :: Maybe (LinkCmpState Unit)) =
                            -- CLink.findTo (RawNode.id rawNode) inletR state.links
                            Patch.linksMap curPatch
                                # Links.findTo (RawNode.id rawNode) inletR
                                # Array.head
                                >>= \link -> Map.lookup (RawLink.id link) state.links
                        outletSrcR = lco.outletId
                        nodeSrcR = lco.nodeId
                        nodeSrcBoxKey = lco.nodeKey
                        outletIndex = lco.index
                        inletTrgR = inletR
                        nodeTrgR = RawNode.id rawNode
                        nodeTrgBoxKey = nodeBoxKey

                    nextPatch /\ isDisconnected <- Actions.disconnectLastAtInlet Actions.Track rawNode inletR curPatch

                    case Patch.findRawNode nodeSrcR nextPatch /\ Patch.findRawNode nodeTrgR nextPatch of -- FIXME: we already have target node here, no need to search for it
                        Just rawNodeSrc /\ Just rawNodeTrg -> do
                            -- CC.log "both nodes were found"

                            Actions.connect
                                Actions.Track
                                { key : nodeSrcBoxKey, node : rawNodeSrc, outlet : outletSrcR, outletIndex }
                                { key : nodeTrgBoxKey, node : rawNodeTrg, inlet  : inletTrgR,  inletIndex  }
                                nextPatch

                        _ -> pure unit

                    pure unit
                else pure unit
            _ -> do
                Actions.tryCallingInletEditor nodeBoxKey rawNode inletR inletIndex mbValueEditorOp

        State.modify_
            (_ { lastClickedOutlet = Nothing })

        Key.mainScreen >~ Screen.render -- FIXME: only re-render patchBox


onLinkClick :: forall id tk pstate fs strepr chrepr mi mo. Wiring mo => Id.PatchR -> Raw.Link -> LinkCmpState Unit -> Line <^> id → {- EventJson → -} BlessedOp (State _ tk pstate fs strepr chrepr mi) mo
onLinkClick = Actions.disconnect Actions.Track