module Cli.Components.NodeBox.InputButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Control.Monad.State (get, modify, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Mark (mark)
import Data.Repr (class FromRepr, class ToRepr, toRepr, fromRepr, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.SProxy (reflect, reflect')
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Data.Text.Format as T
import Data.Text.Output.Blessed (singleLine) as T

import Type.Proxy (Proxy(..))
import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.NodeKey (rawify) as NodeKey
import Blessed.Internal.BlessedSubj (Line)
import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element
import Blessed.UI.Base.Element.Method (setFront) as Element
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Forms.TextArea.Method (setValue) as TextArea
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element

import Cli.Keys as Key
import Cli.Keys (NodeBoxKey, PatchBoxKey, InfoBoxKey, InputButtonKey, mainScreen, statusLine)
import Cli.State (State, LinkState(..), OutputIndex(..), InputIndex(..), logNdfCommandM)
import Cli.State.NwWraper (unwrapN, wrapN)
import Cli.Bounds (collect, inputPos) as Bounds
import Cli.Style (inputsOutputs) as Style
import Cli.Components.Link as Link
import Cli.Components.OutputIndicator as OI
import Cli.Components.InputIndicator as II
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.FullInfoBox as FI
import Cli.Tagging as T

import Noodle.Network as Network
import Noodle.Patch as Patch

import Noodle.Id as Id
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Text.NdfFile.Command as C
import Noodle.Text.NdfFile.Command (commandsToNdf)

import Toolkit.Hydra (Instances, State) as Hydra
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as H
import Toolkit.Hydra.Family.Render.Cli (editorIdOf) as Hydra
import Toolkit.Hydra.Family.Render.Editor (EditorId)



--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall f nstate i din is is' os
     . IsSymbol f
    => Id.HasInput i din is' is
    => ToRepr din H.WrapRepr
    => FromRepr H.WrapRepr din
    -- => HasEditor (Hydra.Cli f) din din Effect
    -- => HasEditor (Hydra.CliD din) is' (Id.Input i) (Noodle.Node f nstate is os Effect) din Effect
    -- => HasEditor' (Hydra.CliD din) (Noodle.Node f nstate is os Effect) i is' is din Effect
    => InputButtonKey
    -> InfoBoxKey
    -> Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> Int
    -> Maybe H.WrapRepr
    -> Signal (Maybe H.WrapRepr)
    -> Proxy din -- H.WrapRepr?
    -> Noodle.Node f nstate is os Effect
    -> Id.Input i
    -> Core.Blessed State
component buttonKey nextInfoBox curPatchId curPatch nextNodeBox idx maybeRepr reprSignal pdin inode inputId =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.input idx inputId maybeRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inputsOutputs
        , Core.on Button.Press
            $ onPress curPatchId curPatch nextNodeBox idx pdin inode inputId $ Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver (Node.family inode) (Id.nodeIdR $ Node.id inode) nextNodeBox nextInfoBox idx inputId maybeRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut nextInfoBox idx
        ]
        []


onMouseOver :: forall i f. IsSymbol i => IsSymbol f => Id.Family' f -> Id.NodeIdR -> NodeBoxKey -> InfoBoxKey -> Int -> Id.Input i -> Maybe H.WrapRepr -> Signal (Maybe H.WrapRepr) -> _ -> _ -> BlessedOp State Effect
onMouseOver family nodeIdR nodeBox infoBox idx inputId _ reprSignal _ _ = do
    state <- State.get
    nodeBounds <- Bounds.collect nodeIdR nodeBox -- FIXME: load from state.locations
    let inputPos = Bounds.inputPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inputId
    infoBox >~ IB.inputInfo inputId
    SL.inputStatus family idx inputId maybeRepr
    FI.inputStatus family idx inputId maybeRepr
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> do
            II.move { x : inputPos.x, y : inputPos.y - 1 }
            II.updateStatus II.Hover
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: InfoBoxKey -> Int ->  _ -> _ -> BlessedOp State Effect
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    FI.clear
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> II.hide
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "out" <> show idx


onPress
    :: forall f nstate i din is is' os
     . IsSymbol f
    => Id.HasInput i din is' is
    => ToRepr din H.WrapRepr
    => FromRepr H.WrapRepr din
    -- => HasEditor (Hydra.CliD din) is' (Id.Input i) (Noodle.Node f nstate is os Effect) din Effect
    -- => HasEditor' (Hydra.CliD din) (Noodle.Node f nstate is os Effect) i is' is din Effect
    => Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> Int
    -> Proxy din
    -> Noodle.Node f nstate is os Effect
    -> Id.Input i
    -> Maybe EditorId
    -> _
    -> _
    -> BlessedOp State Effect
onPress curPatchId curPatch nextNodeBox idx _ inode inputId mbEditorId _ _ =
    {-Id.reflect inputId /\ [] /\ \_ _ -> -} do
        let altIdx = Id.index inputId
        -- liftEffect $ Console.log $ "press" <> show idx
        -- liftEffect $ Console.log $ "apress" <> show altIdx
        let inodeKey = nextNodeBox
        let inodeId = Node.id inode
        state <- State.get
        -- liftEffect $ Console.log $ "handler " <> show idx
        case state.lastClickedOutput of
            Just lco ->
                if inodeKey /= lco.nodeKey then do
                    state <- State.get

                    let maybePrevLink = Map.lookup (NodeKey.rawify inodeKey) state.linksTo >>= Map.lookup (InputIndex idx)

                    let
                        canceler /\ curPatch' =
                            case maybePrevLink of
                                Just (LinkState { inPatch }) -> Patch.forgetLink' inPatch curPatch
                                Nothing -> pure unit /\ curPatch

                    _ <- liftEffect canceler

                    case maybePrevLink of
                        Just linkState ->
                            Key.patchBox >~ Link.remove linkState
                        Nothing -> pure unit

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

                    -- State.modify_ $ Link.store linkCmp
                    State.modify_ $ Link.store linkCmp

                    Key.patchBox >~ Link.append linkCmp

                    let onodeId = Id.withNodeId lco.nodeId reflect'

                    logNdfCommandM $ Cmd.Connect (C.nodeId onodeId) (C.outputIndex lco.index) (C.nodeId $ reflect' inodeId) (C.inputIndex idx) -- TODO: log somewhere else in a special place
                    -- FIXME: duplicates `CommandLogBox.refresh`, done due to cycle in dependencies
                    state' <- State.get
                    Key.commandLogBox >~ Box.setContent $ T.singleLine $ NdfFile.toTaggedNdfCode state'.commandLog
                    -- Key.commandLogBox >~ Box.setContent $ NdfFile.toNdfCode state.commandLog
                    -- END

                    State.modify_  $ \s -> s
                        { network = wrapN $ Network.withPatch curPatchId (const nextPatch') $ unwrapN $ s.network
                        , linkWasMadeHack = true
                        }

                    linkCmp # Link.on Element.Click (onLinkClick holdsLink)

                    OI.hide
                else pure unit
            Nothing -> do
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
                            {- let mbEditor = Hydra.createEditorFor (H.Value $ H.Number 0.0) Key.patchBox (const $ pure unit)
                            case mbEditor of
                                Just editor -> do
                                    editor
                                    -- Key.mainScreen >~ Screen.render
                                Nothing -> pure unit -}
                            -- TODO: Multiple operations operator
                            editor >~ Element.setTop $ Offset.px $ inodeBounds.top - 1
                            editor >~ Element.setLeft $ Offset.px $ inodeBounds.left
                            editor >~ TextArea.setValue ""
                            editor >~ Element.setFront
                            editor >~ Element.show
                            State.modify_  (_ { linkWasMadeHack = false })
                        else
                            State.modify_  (_ { linkWasMadeHack = false })
                        --Key.numValueEditor >~ Element.focus
                    Nothing ->
                        pure unit
        State.modify_
            (_ { lastClickedOutput = Nothing })
        Key.mainScreen >~ Screen.render -- FIXME: only re-render patchBox
    -- onInputSelect nodeId input nextNodeBox idx (Id.reflect input)


-- TODO: move to Link module?
onLinkClick :: forall id. Patch.HoldsLink -> LinkState -> Line <^> id → EventJson → BlessedOp State Effect
onLinkClick holdsLink linkState _ _ = do
    -- liftEffect $ Console.log "click link"
    Patch.withLink holdsLink Node.unsafeDisconnect
    Key.patchBox >~ Link.remove linkState
    State.modify_ $ Link.forget linkState
    Key.mainScreen >~ Screen.render