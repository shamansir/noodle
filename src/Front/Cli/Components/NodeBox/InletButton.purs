module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

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

import Blessed.Internal.NodeKey (rawify) as NodeKey
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
import Cli.State (State) {- LinkState(..), OutletIndex(..), InputIndex(..), logNdfCommandM)  -}
import Cli.Style (inletsOutlets) as Style

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.Link (LinkState)
import Cli.Components.Link (remove) as CLink

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Wiring (class Wiring)
import Noodle.Patch (findRawLink, disconnectRaw) as Patch


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs repr m
     . T.At T.StatusLine repr
    => T.At T.ChannelLabel repr
    => Patch pstate fs repr m
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Id.FamilyR -> Id.NodeR -> Id.InletR
    -> Int
    -> Maybe repr
    -> Signal repr
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs repr m)
component curPatch buttonKey nodeBoxKey infoBoxKey familyR nodeR inletR idx mbRepr reprSignal =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.inlet idx inletR mbRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        -- REM , Core.on Button.Press
        -- REM     $ onPress curPatchId curPatch nextNodeBox idx pdin inode inletId $ Hydra.editorIdOf =<< maybeRepr

        , Core.on Element.MouseOver
            $ onMouseOver familyR nodeR nodeBoxKey infoBoxKey idx inletR mbRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey idx
        ]
        []


onMouseOver
    :: forall tk pstate fs repr m
     . T.At T.StatusLine repr
    => Id.FamilyR
    -> Id.NodeR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Id.InletR
    -> Maybe repr
    -> Signal repr
    -> _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
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


onMouseOut :: forall tk pstate fs repr m. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
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
    :: forall tk pstate fs repr m
     . Wiring m
    => Id.PatchR
    -> Patch pstate fs repr m
    -> NodeBoxKey
    -> Int
    -> Id.NodeR
    -> Id.InletR
    -> _
    -> _
    -> BlessedOp (State tk pstate fs repr m) Effect
onPress patchR curPatch nodeBoxKey idx nodeR inletR _ _ = do
        state <- State.get
        case state.lastClickedOutlet of
            Just lco ->
                if nodeBoxKey /= lco.nodeKey then do

                    let
                        (mbPrevLink :: Maybe (LinkState Unit)) =
                            Map.lookup (NodeKey.rawify nodeBoxKey) state.linksTo
                            >>= Map.lookup (Id.InletIndex idx)

                    nextPatch /\ isDisconnected <-
                        case mbPrevLink of
                            Just linkState ->
                                let
                                    linkId = _.inPatch $ unwrap linkState
                                in
                                    case curPatch # Patch.findRawLink linkId of
                                        Just rawLink -> do
                                            nextPatch /\ success <- liftEffect (curPatch # Patch.disconnectRaw rawLink)
                                            -- FIXME: w/o `unsafeCoerce` breaks type of State in the logic, because `LinksState Unit` confronts
                                            -- with `LinkState s` <-> `BlessedOp s m` in `CLink.remove`.
                                            -- And for the moment there is no way in `Blessed` to get rid of `State` in SNode because of many reasons including the way Handlers currently work.
                                            Key.patchBox >~ CLink.remove (unsafeCoerce linkState)
                                            pure $ nextPatch /\ success
                                        Nothing -> pure (curPatch /\ false)

                            Nothing -> pure (curPatch /\ false)

                    {- REM
                    linkId /\ nextPatch' /\ holdsLink <- liftEffect $ Node.withOutputInNodeMRepr
                        (lco.outputId :: Node.HoldsOutputInNodeMRepr Effect H.WrapRepr) -- w/o type given here compiler fails to resolve constraints somehow
                        (\_ onode outputId -> do
                            link <- Node.connectByRepr (Proxy :: _ H.WrapRepr) outputId inputId onode inode
                            let linkId /\ nextPatch' = Patch.registerLink link curPatch'
                            pure $ linkId /\ nextPatch' /\ Patch.holdLink link
                        )

                    linkCmp <- Link.create
                                linkId
                                { key : lco.nodeKey, id : Id.withNodeId lco.nodeId Id.nodeIdR }
                                (OutputIndex lco.index)
                                { key : inodeKey, id : Id.nodeIdR inodeId }
                                (InputIndex idx)

                    State.modify_ $ Link.store linkCmp

                    Key.patchBox >~ Link.append linkCmp

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
            Nothing -> do
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
        mainScreen >~ Screen.render -- FIXME: only re-render patchBox
