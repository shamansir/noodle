module Cli.Components.NodeBox where

import Prelude
import Debug as Debug

import Cli.WsServer as WSS

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.State as State
import Control.Monad.State (class MonadState)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (length) as List
import Data.KeyHolder as KH
import Data.Repr (class FromToReprRow, class ToReprRow)
import Data.Symbol (class IsSymbol)
import Data.String as String
import Data.Mark (mark)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.SProxy (reflect, reflect')
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.FromToFile (class Encode, encode)

import Signal (Signal, (~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel

import Blessed ((>~))
import Blessed as B
import Blessed.Tagger (fgc, bgc, s, fgcs) as T
import Blessed.Tagger (render) as T

import Blessed.Core.Border as Border
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp (runM, runM', getStateRef, imapState) as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
-- import Blessed.UI.Line.Li ()

import Noodle.Id as Id
import Noodle.Toolkit as Toolkit
import Noodle.Toolkit.Has as THas
import Noodle.Patch as Patch
import Noodle.Patch (Patch) as Noodle
import Noodle.Patch.Has as PHas
import Noodle.Patch.Is as PIs
import Noodle.Node as Node
import Noodle.Node (Node) as Noodle
import Noodle.Family.Def as Family
import Noodle.Node.MapsFolds.Repr
    ( class ToReprHelper, class ToReprFoldToMapsHelper
    , Repr(..)
    , nodeToRepr, nodeToMapRepr
    , subscribeReprChanges, subscribeReprMapChanges
    ) as R
import Noodle.Node.MapsFolds.Flatten as R
import Noodle.Node.HoldsNodeState (HoldsNodeState, class IsNodeState, default, fromGlobal)
import Noodle.Fn.Protocol (ChangeFocus(..))
import Noodle.Stateful (get, getM, setM) as Stateful
import Noodle.Text.NdfFile.Command as Cmd


import Cli.Keys (NodeBoxKey, PatchBoxKey)
import Cli.Keys (mainScreen, patchBox, statusLine) as Key
import Cli.Palette as Palette
import Cli.Palette.Item (crepr) as Palette
import Cli.State (State, logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.Style as Style
import Cli.Tagging as T
import Cli.Components.Link as Link
import Cli.Components.NodeBox.InputsBox as InputsBox
import Cli.Components.NodeBox.OutputsBox as OutputsBox
import Cli.Components.NodeBox.InputButton as InputButton
import Cli.Components.NodeBox.OutputButton as OutputButton
import Cli.Components.NodeBox.RemoveButton as RemoveButton
import Cli.Components.NodeBox.InfoBox as InfoBox
import Cli.Components.NodeBox.HasBody (class HasCliBody, class HasCliCustomSize, cliSize)
import Cli.Components.NodeBox.HasBody (runBlessed) as NodeBody
import Cli.Components.CommandLogBox as CommandLogBox
import Cli.Components.HydraCodeBox as HydraCodeBox
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.FullInfoBox as FI
import Cli.Bounds as Bounds

import Toolkit.Hydra (Families, Instances, State, Toolkit) as Hydra
import Toolkit.Hydra.Group (toGroup) as Hydra
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra.Repr.Info (InfoRepr) as Hydra
import Toolkit.Hydra.Family.Render.Cli (CliF) as Hydra
import Toolkit.Hydra.Lang as Lang
import Toolkit.Hydra.Lang.ToCode as Lang


width :: String -> Int -> Int -> Dimension
width familyName isCount osCount =
    Dimension.px $ widthN familyName isCount osCount


widthN :: String -> Int -> Int -> Int
widthN familyName isCount osCount =
    (max (String.length familyName) $ max (InputsBox.widthN isCount) (OutputsBox.widthN osCount)) + 4


-- widthN :: String -> Int -> Int -> Dimension


autoPos :: forall m. BlessedOpM State m (Int /\ Int)
autoPos = do
    state <- State.get

    let topN = state.lastShiftX + 2
    let leftN = 16 + state.lastShiftY + 2
    pure (leftN /\ topN)


fromNodeAuto
    :: forall instances' rlins f state isrl is osrl os repr_is repr_os
     . PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
    => Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Noodle.Node f state is os Effect
    -> BlessedOpM State Effect _
fromNodeAuto curPatchId curPatch family node =
    autoPos >>= \pos -> fromNodeAt pos curPatchId curPatch family node


fromNodeAt
    :: forall instances' rlins f state isrl is osrl os repr_is repr_os
     . PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
    => Int /\ Int
    -> Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Noodle.Node f state is os Effect
    -> BlessedOpM State Effect _
fromNodeAt (leftN /\ topN) curPatchId curPatch family node = do
    --liftEffect $ Node.run node -- just Node.run ??
    liftEffect $ Node.runOnInputUpdates node
    liftEffect $ Node.runOnStateUpdates node

    state <- State.get

    let (updates :: Signal (ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr)) = R.subscribeReprMapChanges (R.Repr :: _ Hydra.WrapRepr) node
    let (updates' :: Signal (R.NodeLineMap Hydra.WrapRepr)) = map Tuple.snd updates
    let nextNodeBox = NodeKey.next state.lastKeys.nodeBox
    let nextInputsBox = NodeKey.next state.lastKeys.inputsBox
    let nextOutputsBox = NodeKey.next state.lastKeys.outputsBox
    let nextInfoBox = NodeKey.next state.lastKeys.infoBox
    let nextRemoveButton = NodeKey.next state.lastKeys.removeButton

    let mbBodySize = cliSize (Proxy :: _ (Hydra.CliF f)) nextNodeBox node

    let top = Offset.px topN
    let left = Offset.px leftN

    (nodeId /\ _ /\ inputsReps /\ outputReprs) <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node

    logNdfCommandM $ Cmd.MakeNode (Cmd.family $ reflect family) (Cmd.coord topN) (Cmd.coord $ leftN) (Cmd.nodeId $ reflect' nodeId) -- TODO: log somewhere else in a special place
    CommandLogBox.refresh

    let (is :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeInputsTest' node
    let (os :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeOutputsTest' node

    let (nodeHolder :: Patch.HoldsNode Effect) = Patch.holdNode curPatch node
    let (toInputSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.InputR -> Maybe Hydra.WrapRepr)) = map R.getInputFromMap
    let (toOutputSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.OutputR -> Maybe Hydra.WrapRepr)) = map R.getOutputFromMap
    let isWithReprs = (\hiinr -> Node.withInputInNodeMRepr hiinr (\_ _ inputId -> Map.lookup (Id.inputR inputId) inputsReps) /\ hiinr) <$> is
    let osWithReprs = (\hoinr -> Node.withOutputInNodeMRepr hoinr (\_ _ outputId -> Map.lookup (Id.outputR outputId) outputReprs) /\ hoinr) <$> os

    -- TODO: probably use Repr to create input bars and output bars, this way using Input' / Output' instances, we will probably be able to connect things
    --       or not Repr but some fold over inputs / outputs shape
    --       but the question remains: when we have some selected input for the receiving node in the handler, wherefrom do we get the node id of the output?
    --       we have the family encoded as symbol and hash of the is the thing that changes in real-time
    --       so we need to recreate the family. In case of Hydra, we have access to families' symbols but also by symbols.
    --       we have `lastClickedOtlet` in the state.
    --       Maybe try using `Exists` as we're sure the Node Family exists but don't want to parametrize `State` type with it.

    -- let is /\ os = Record.keys (rec.inputs :: Record is) /\ Record.keys (rec.outputs :: Record os)

    let
        (nodeIdR :: Id.NodeIdR) = Id.nodeIdR $ Node.id node
        boxWidth =
            case mbBodySize of
                Just { width } -> width - 1
                Nothing -> widthN (reflect family) (Array.length is) (Array.length os)
        boxHeight =
            case mbBodySize of
                Just { width, height } -> height + 2
                Nothing -> 5
        outputsTopOffset =
            Offset.px $
                case mbBodySize of
                    Just { height } -> height - 1
                    Nothing -> 2
        removeButtonOffset =
            Offset.px $
                case mbBodySize of
                    Just { height } -> height
                    Nothing -> 3
        inputsKeys /\ inputsBoxN =
            InputsBox.component curPatchId curPatch nextNodeBox nextInfoBox nextInputsBox family (toInputSignal updates') isWithReprs
        outputsKeys /\ outputsBoxN =
            OutputsBox.component outputsTopOffset nodeHolder nextNodeBox nextInfoBox nextOutputsBox (toOutputSignal updates') osWithReprs
        infoBoxN =
            InfoBox.component nextInfoBox $ boxWidth - 2
        removeButtonN =
            RemoveButton.component removeButtonOffset family node nextNodeBox nextInfoBox nextRemoveButton
        nextNodeBoxN =
            B.box nextNodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px boxWidth
                , Box.height $ Dimension.px boxHeight
                , Box.label $ T.render $ T.nodeLabel family
                , Box.tags true
                , Style.nodeBoxBorder
                , Style.nodeBox
                , Core.on Element.Move
                    $ onMove nodeIdR nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
                , Core.on Element.MouseOver
                    $ onMouseOver family
                , Core.on Element.MouseOut
                    $ onMouseOut
                ]
                [ ]
        {-
        inputText =
            B.textBox Key.textBox
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                , Box.content "text"
                , Box.width $ Dimension.percents 90.0
                , Box.height $ Dimension.px 1
                , Style.inputBox
                , TextArea.mouse true
                , TextArea.inputOnFocus true
                ]
                [ ]
                -}
        renderNodeUpdate :: forall a. ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr -> BlessedOp a Effect -- FIXME: shouldn't there be node state? but it's not used in the function anyway
        renderNodeUpdate = renderUpdate nextNodeBox inputsKeys outputsKeys

    (stateRef :: Ref State) <- Blessed.getStateRef

    (nodeState :: state) <- liftEffect $ Stateful.getM node

    --renderNodeUpdate mapRepr
    liftEffect $ Signal.runSignal $ updates ~> (Blessed.runM unit <<< renderNodeUpdate) -- FIXME: shouldn't there be node state? but it's not used in the function anyway
    liftEffect $ Signal.runSignal $ updates ~> logDataCommand stateRef

    liftEffect $ when (Lang.producesCode family) $ Signal.runSignal $ updates ~> updateCodeFor stateRef family

    -- liftEffect $ Node.listenUpdatesAndRun node
    liftEffect $ Node.run node

    Key.patchBox >~ Node.append nextNodeBoxN

    nextNodeBox >~ Node.append inputsBoxN
    nextNodeBox >~ Node.append infoBoxN
    nextNodeBox >~ Node.append outputsBoxN
    nextNodeBox >~ Node.append removeButtonN

    mapRepr2 <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node
     -- FIXME: why it doesn't apply values for `osc` node and for any other too, as I get it? for example: default `osc` out value stays in UI, however log is performed from its `process` function (yeah?)
    -- liftEffect $ Console.log $ show atOut
    -- liftEffect $ Console.log $ show mapRepr
    -- liftEffect $ Console.log $ show mapRepr2
    -- FIXME: try logging/tracking all NodeLineRec updates

    renderNodeUpdate $ Everything /\ mapRepr2

    (nodeStateRef :: Ref state) <- liftEffect $ Ref.new nodeState

    -- run :: Proxy x -> NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m
    -- _ <- Blessed.imapState ?wh ?wh $ NodeBody.run (Proxy :: _ (Hydra.Cli f)) nextNodeBox node
    liftEffect $ Blessed.runM' nodeStateRef $ NodeBody.runBlessed (Proxy :: _ (Hydra.CliF f)) nextNodeBox node

    -- nextNodeBox >~ Node.append inputText

    let
        lastKeys =
            { nodeBox : nextNodeBox
            , inputsBox : nextInputsBox
            , outputsBox : nextOutputsBox
            , infoBox : nextInfoBox
            , removeButton : nextRemoveButton
            }

        location =
            { left : leftN
            , top : topN
            , width : boxWidth
            , height : boxHeight
            }

    State.modify_ (_
        { lastShiftX = state.lastShiftX + 1
        , lastShiftY = state.lastShiftY + 1
        , lastKeys = lastKeys
        , nodeKeysMap = Map.insert nodeId nextNodeBox state.nodeKeysMap
        , locations = Map.insert nodeId location state.locations
        }
    )

    Key.mainScreen >~ Screen.render

    pure { nextNodeBoxN, inputsBoxN, outputsBoxN, nextNodeBox }



fromFamilyAt
    :: forall families' instances' rlins f state isrl is osrl os repr_is repr_os
     . THas.HasNodesOf families' (Hydra.Families Effect) instances' (Hydra.Instances Effect) f state isrl is osrl os Effect
    => PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
    => Int /\ Int
    -> Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Hydra.Toolkit Effect
    -> BlessedOpM State Effect _
fromFamilyAt pos curPatchId curPatch family _ tk = do
    (node :: Noodle.Node f state is os Effect) <- liftEffect $ Toolkit.spawn tk family
    let (mbState :: Maybe state) = fromGlobal $ Stateful.get curPatch
    node' <- liftEffect $ case mbState of
        Just state -> Stateful.setM state node
        Nothing -> pure node
    -- TODO: update node in the patch?
    fromNodeAt pos curPatchId curPatch family node'


fromFamilyAuto
    :: forall families' instances' rlins f state isrl is osrl os repr_is repr_os
     . THas.HasNodesOf families' (Hydra.Families Effect) instances' (Hydra.Instances Effect) f state isrl is osrl os Effect
    => PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
    => Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Hydra.Toolkit Effect
    -> BlessedOpM State Effect _
fromFamilyAuto curPatchId curPatch family def tk = do
    autoPos >>= \pos -> fromFamilyAt pos curPatchId curPatch family def tk


logDataCommand
    :: forall m
     . MonadEffect m
    => Ref State
    -> ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr
    -> m Unit
logDataCommand stateRef (chFocus /\ nodeId /\ _ /\ inputs /\ outputs) =
    case chFocus of
        InputChange input ->
            case Map.lookup input inputs of
                Just wrapRepr -> do
                    flip logNdfCommandByRef stateRef $ Cmd.Send (Cmd.nodeId $ reflect' nodeId) (Cmd.inputAlias $ reflect' input) $ Cmd.encodedValue $ encode wrapRepr
                    liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                Nothing -> pure unit
        OutputChange output ->
            case Map.lookup output outputs of
                Just wrapRepr -> do
                    flip logNdfCommandByRef stateRef $ Cmd.SendO (Cmd.nodeId $ reflect' nodeId) (Cmd.outputAlias $ reflect' output) $ Cmd.encodedValue $ encode wrapRepr
                    liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                Nothing -> pure unit
        _ -> pure unit


updateCodeFor
    :: forall m f
     . MonadEffect m
    => IsSymbol f
    => Ref State
    -> Id.Family f
    -> ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr
    -> m Unit
updateCodeFor stateRef family update@(_ /\ nodeId /\ _) = do
    flip (logLangCommandByRef nodeId) stateRef $ Lang.updateToCommand family $ Tuple.snd update
    liftEffect $ Blessed.runM' stateRef HydraCodeBox.refresh -- FIXME: use `Blessed.impairN`
    state <- liftEffect $ Ref.read stateRef
    case state.wsServer of
        Just serverState ->
            liftEffect $ flip WSS.broadcastProgram serverState $ Lang.formProgram state.program
        Nothing -> pure unit


renderUpdate
    :: forall state
     . NodeBoxKey
    -> InputsBox.KeysMap
    -> OutputsBox.KeysMap
    -> ChangeFocus /\ Id.NodeIdR /\ Hydra.WrapRepr /\ Map Id.InputR Hydra.WrapRepr /\ Map Id.OutputR Hydra.WrapRepr
    -> BlessedOp state Effect
renderUpdate _ inputsKeysMap outputsKeysMap (_ /\ nodeId /\ stateRepr /\ inputsReps /\ outputReprs) = do
    -- liftEffect $ Console.log $ show outputReprs
    _ <- traverseWithIndex updateInput inputsReps
    _ <- traverseWithIndex updateOutput outputReprs
    Key.mainScreen >~ Screen.render
    where
        updateInput inputR repr =
            case Map.lookup inputR inputsKeysMap of
                Just inputKey -> do
                    inputKey >~ Box.setContent $ T.render $ T.input' 0 inputR $ Just repr
                Nothing -> pure unit
        updateOutput outputR repr =
            case Map.lookup outputR outputsKeysMap of
                Just outputKey -> do
                    outputKey >~ Box.setContent $ T.render $ T.output' 0 outputR $ Just repr
                Nothing -> pure unit


onMove :: Id.NodeIdR -> NodeBoxKey -> NodeBoxKey -> EventJson -> BlessedOp State Effect
onMove nodeId nodeKey _ _ = do
    let rawNk = NodeKey.rawify nodeKey
    newBounds <- Bounds.collect nodeId nodeKey
    state <- State.modify \s -> s { locations = Map.update (updatePos newBounds) nodeId s.locations }
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) Link.update
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) Link.update
    where
        updatePos nb = Just <<< Bounds.move nb


onMouseOver :: forall f. IsSymbol f => Id.Family f -> _ -> _ -> BlessedOp State Effect
onMouseOver family _ _ = do
    -- maybeRepr <- liftEffect $ Signal.get reprSignal
    -- -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inputId
    SL.familyStatus family
    FI.familyStatus family
    Key.mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: _ -> _ -> BlessedOp State Effect
onMouseOut _ _ = do
    SL.clear
    FI.clear
    Key.mainScreen >~ Screen.render