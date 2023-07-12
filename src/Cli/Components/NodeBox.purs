module Cli.Components.NodeBox where

import Prelude

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
import Noodle.Toolkit3 as Toolkit
import Noodle.Toolkit3.Has (class HasNodesOf) as Toolkit
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch) as Noodle
import Noodle.Patch4.Has as Has
import Noodle.Node2 as Node
import Noodle.Node2 (Node) as Noodle
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr
    ( class ToReprHelper, class ToReprFoldToMapsHelper
    , Repr(..)
    , nodeToRepr, nodeToMapRepr
    , subscribeReprChanges, subscribeReprMapChanges
    ) as R
import Noodle.Node2.MapsFolds.Flatten as R
import Noodle.Fn2.Protocol (ChangeFocus(..))


import Cli.Keys (NodeBoxKey)
import Cli.Keys (mainScreen, patchBox, statusLine) as Key
import Cli.Palette as Palette
import Cli.Palette.Item (crepr) as Palette
import Cli.State (State, logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.Style as Style
import Cli.Components.Link as Link
import Cli.Components.NodeBox.InletsBox as InletsBox
import Cli.Components.NodeBox.OutletsBox as OutletsBox
import Cli.Components.NodeBox.InletButton as InletButton
import Cli.Components.NodeBox.OutletButton as OutletButton
import Cli.Components.NodeBox.HasBody (class HasBody, class HasBody')
import Cli.Components.NodeBox.HasBody (run, run') as NodeBody
import Cli.Components.NodeBox.HoldsNodeState (HoldsNodeState, class IsNodeState, default)

import Noodle.Text.NetworkFile.Command as Cmd

import Toolkit.Hydra2 (Families, Instances, State, Toolkit) as Hydra
import Toolkit.Hydra2.Group (toGroup) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Repr.Info (InfoRepr) as Hydra
import Toolkit.Hydra2.Family.Render.Cli (Cli) as Hydra
import Toolkit.Hydra2.Lang as Lang
import Toolkit.Hydra2.Lang.ToCode as Lang


width :: String -> Int -> Int -> Dimension
width familyName isCount osCount =
    Dimension.px $ widthN familyName isCount osCount


widthN :: String -> Int -> Int -> Int
widthN familyName isCount osCount =
    (max (String.length familyName) $ max (InletsBox.widthN isCount) (OutletsBox.widthN osCount)) + 4


-- widthN :: String -> Int -> Int -> Dimension

fromNode
    :: forall f state rli is rlo os repr_is repr_os instances'
     . Has.HasInstancesOf f instances' (Hydra.Instances Effect) (Array (Noodle.Node f state is os Effect))
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    => R.ToReprFoldToMapsHelper f is rli os rlo Hydra.WrapRepr state
    => FromToReprRow rli is Hydra.WrapRepr
    => FromToReprRow rlo os Hydra.WrapRepr
    => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    => HasBody (Hydra.Cli f) f state is os Effect
    => IsNodeState state
    => Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Noodle.Node f state is os Effect
    -> BlessedOpM State Effect _
fromNode curPatchId curPatch family node = do
    liftEffect $ Node.listenUpdatesAndRun node -- just Node.run ??

    state <- State.get

    let (updates :: Signal (ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr)) = R.subscribeReprMapChanges (R.Repr :: _ Hydra.WrapRepr) node
    let (updates' :: Signal (R.NodeLineMap Hydra.WrapRepr)) = map Tuple.snd updates
    let nextNodeBox = NodeKey.next state.lastKeys.nodeBox
    let nextInletsBox = NodeKey.next state.lastKeys.inletsBox
    let nextOutletsBox = NodeKey.next state.lastKeys.outletsBox
    let nextInfoBox = NodeKey.next state.lastKeys.infoBox

    let topN = state.lastShiftX + 2
    let leftN = 16 + state.lastShiftY + 2
    let top = Offset.px topN
    let left = Offset.px leftN

    (nodeId /\ _ /\ inputsReps /\ outputReprs) <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node

    logNdfCommandM $ Cmd.MakeNode (reflect family) topN leftN $ reflect' nodeId -- TODO: log somewhere else in a special place

    let (is :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeInputsTest' node
    let (os :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeOutputsTest' node

    let (nodeHolder :: Patch.HoldsNode Effect) = Patch.holdNode curPatch node
    let (toInputSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.InputR -> Maybe Hydra.WrapRepr)) = map R.getInputFromMap
    let (toOutputSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.OutputR -> Maybe Hydra.WrapRepr)) = map R.getOutputFromMap
    let isWithReprs = (\hiinr -> Node.withInputInNodeMRepr hiinr (\_ _ inputId -> Map.lookup (Id.inputR inputId) inputsReps) /\ hiinr) <$> is
    let osWithReprs = (\hoinr -> Node.withOutputInNodeMRepr hoinr (\_ _ outputId -> Map.lookup (Id.outputR outputId) outputReprs) /\ hoinr) <$> os

    -- TODO: probably use Repr to create inlet bars and outlet bars, this way using Input' / Output' instances, we will probably be able to connect things
    --       or not Repr but some fold over inputs / outputs shape
    --       but the question remains: when we have some selected input for the receiving node in the handler, wherefrom do we get the node id of the output?
    --       we have the family encoded as symbol and hash of the is the thing that changes in real-time
    --       so we need to recreate the family. In case of Hydra, we have access to families' symbols but also by symbols.
    --       we have `lastClickedOtlet` in the state.
    --       Maybe try using `Exists` as we're sure the Node Family exists but don't want to parametrize `State` type with it.

    -- let is /\ os = Record.keys (rec.inputs :: Record is) /\ Record.keys (rec.outputs :: Record os)

    let
        boxWidth = widthN (reflect family) (Array.length is) (Array.length os)
        inletsKeys /\ inletsBoxN =
            InletsBox.component curPatchId curPatch nextNodeBox nextInfoBox nextInletsBox family (toInputSignal updates') isWithReprs
        outletsKeys /\ outletsBoxN =
            OutletsBox.component nodeHolder nextNodeBox nextInfoBox nextOutletsBox (toOutputSignal updates') osWithReprs
        infoBoxN =
            B.box nextInfoBox
                [ Box.top $ Offset.px 1
                , Box.left $ Offset.px 0
                -- , Box.width $ Dimension.calc $ C.px boxWidth <-> C.px 2 -- FIXME: didn't work
                , Box.width $ Dimension.px $ boxWidth - 2
                , Box.height $ Dimension.px 1
                , Box.tags true
                , Box.content ""
                , Style.infoBox
                ]
                [ ]
        nextNodeBoxN =
            B.box nextNodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px boxWidth
                , Box.height $ Dimension.px 5
                , Box.label $ toLabel family
                , Box.tags true
                , Style.nodeBoxBorder
                , Style.nodeBox
                , Core.on Element.Move $ onMove nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
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
        renderNodeUpdate :: forall a. ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr -> BlessedOp a Effect
        renderNodeUpdate = renderUpdate nextNodeBox inletsKeys outletsKeys

    (stateRef :: Ref State) <- Blessed.getStateRef

    --renderNodeUpdate mapRepr
    liftEffect $ Signal.runSignal $ updates ~> (Blessed.runM unit <<< renderNodeUpdate)
    liftEffect $ Signal.runSignal $ updates ~> logDataCommand stateRef

    liftEffect $ when (Lang.producesCode family) $ Signal.runSignal $ updates ~> updateCodeFor stateRef family

    -- liftEffect $ Node.listenUpdatesAndRun node
    -- liftEffect $ Node.run node

    Key.patchBox >~ Node.append nextNodeBoxN
    nextNodeBox >~ Node.append inletsBoxN
    nextNodeBox >~ Node.append infoBoxN
    nextNodeBox >~ Node.append outletsBoxN

    mapRepr2 <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node
     -- FIXME: why it doesn't apply values for `osc` node and for any other too, as I get it? for example: default `osc` out value stays in UI, however log is performed from its `process` function (yeah?)
    -- liftEffect $ Console.log $ show atOut
    -- liftEffect $ Console.log $ show mapRepr
    -- liftEffect $ Console.log $ show mapRepr2
    -- FIXME: try logging/tracking all NodeLineRec updates

    renderNodeUpdate $ Everything /\ mapRepr2

    (nodeStateRef :: Ref state) <- liftEffect $ Ref.new default

    -- run :: Proxy x -> NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m
    -- _ <- Blessed.imapState ?wh ?wh $ NodeBody.run (Proxy :: _ (Hydra.Cli f)) nextNodeBox node
    liftEffect $ Blessed.runM' nodeStateRef $ NodeBody.run (Proxy :: _ (Hydra.Cli f)) nextNodeBox node

    -- nextNodeBox >~ Node.append inputText

    let
        lastKeys =
            { nodeBox : nextNodeBox
            , inletsBox : nextInletsBox
            , outletsBox : nextOutletsBox
            , infoBox : nextInfoBox
            }

    State.modify_ (_
        { lastShiftX = state.lastShiftX + 1
        , lastShiftY = state.lastShiftY + 1
        , lastKeys = lastKeys
        , nodeKeysMap = Map.insert nodeId nextNodeBox state.nodeKeysMap
        }
    )

    Key.mainScreen >~ Screen.render

    pure { nextNodeBoxN, inletsBoxN, outletsBoxN, nextNodeBox }



fromFamily
    :: forall f state fs iis rli is rlo os repr_is repr_os
     . Toolkit.HasNodesOf (Hydra.Families Effect) (Hydra.Instances Effect) f state fs iis rli is rlo os Effect
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    => R.ToReprFoldToMapsHelper f is rli os rlo Hydra.WrapRepr state
    => FromToReprRow rli is Hydra.WrapRepr
    => FromToReprRow rlo os Hydra.WrapRepr
    => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    => HasBody (Hydra.Cli f) f state is os Effect
    => IsNodeState state
    => Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Hydra.Toolkit Effect
    -> BlessedOpM State Effect _
fromFamily curPatchId curPatch family _ tk = do
    (node :: Noodle.Node f state is os Effect) <- liftEffect $ Toolkit.spawn tk family

    fromNode curPatchId curPatch family node


logDataCommand
    :: forall m
     . MonadEffect m
    => Ref State
    -> ChangeFocus /\ R.NodeLineMap Hydra.WrapRepr
    -> m Unit
logDataCommand stateRef (chFocus /\ nodeId /\ _ /\ _ /\ outputs) =
    case chFocus of
        OutputChange output ->
            case Map.lookup output outputs of
                Just wrapRepr ->
                    flip logNdfCommandByRef stateRef $ Cmd.SendO_ (reflect' nodeId) (reflect' output) $ encode wrapRepr
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
updateCodeFor stateRef family update@(_ /\ nodeId /\ _) =
    flip (logLangCommandByRef nodeId) stateRef $ Lang.updateToCommand family $ Tuple.snd update


renderUpdate
    :: forall a
     . NodeBoxKey
    -> InletsBox.KeysMap
    -> OutletsBox.KeysMap
    -> ChangeFocus /\ Id.NodeIdR /\ Hydra.WrapRepr /\ Map Id.InputR Hydra.WrapRepr /\ Map Id.OutputR Hydra.WrapRepr
    -> BlessedOp a Effect
renderUpdate _ inputsKeysMap outputsKeysMap (_ /\ nodeId /\ stateRepr /\ inputsReps /\ outputReprs) = do
    -- liftEffect $ Console.log $ show outputReprs
    _ <- traverseWithIndex updateInput inputsReps
    _ <- traverseWithIndex updateOutput outputReprs
    Key.mainScreen >~ Screen.render
    where
        updateInput inputR repr =
            case Map.lookup inputR inputsKeysMap of
                Just inputKey -> do
                    inputKey >~ Box.setContent $ InletButton.content' 0 inputR $ Just repr
                Nothing -> pure unit
        updateOutput outputR repr =
            case Map.lookup outputR outputsKeysMap of
                Just outputKey -> do
                    outputKey >~ Box.setContent $ OutletButton.content' 0 outputR $ Just repr
                Nothing -> pure unit


onMove :: NodeBoxKey -> NodeBoxKey → EventJson → BlessedOp State Effect
onMove nodeKey _ _ = do
    state <- State.get
    let rawNk = NodeKey.rawify nodeKey
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) Link.update
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) Link.update


toLabel :: forall f. IsSymbol f => Id.Family f -> String
toLabel family =
    let color = mark $ Hydra.toGroup family
    in T.render $ T.bgc (Palette.crepr Palette.nodeBg) $ T.fgc color $ T.s $ Id.reflectFamily family


onMouseOver :: forall f. IsSymbol f => Id.Family f -> _ -> _ -> BlessedOp State Effect
onMouseOver family _ _ = do
    -- maybeRepr <- liftEffect $ Signal.get reprSignal
    -- -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inputId
    Key.statusLine >~ Box.setContent $ T.render $ T.fgcs (Palette.crepr Palette.familyName) $ reflect family
    Key.mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: _ -> _ -> BlessedOp State Effect
onMouseOut _ _ = do
    Key.statusLine >~ Box.setContent ""
    Key.mainScreen >~ Screen.render