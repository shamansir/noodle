module Cli.Components.NodeBox where

import Prelude

import Data.Text.Format (fgc, bgc, s, fgcs) as T
import Data.Text.Output.Blessed (singleLine) as T

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
-- REM import Data.KeyHolder as KH
import Data.Symbol (class IsSymbol)
import Data.String as String
import Data.TraversableWithIndex (traverseWithIndex)
-- REM import Data.SProxy (reflect, reflect')
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
-- REM import Data.FromToFile (class Encode, encode)

import Signal (Signal, (~>))
import Signal as Signal
import Signal.Extra as SignalX
import Signal.Channel (Channel)
import Signal.Channel as Channel

import Blessed ((>~))
import Blessed as B


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
import Blessed.Internal.BlessedOp (runM, runM', getStateRef, imapState, lift, lift') as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
-- import Blessed.UI.Line.Li ()

import Noodle.Repr (class HasFallback, class FromRepr, class ToRepr)
import Noodle.Id as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Patch as Patch
import Noodle.Patch (Patch) as Noodle
import Noodle.Node as Node
import Noodle.Node (Node) as Noodle
import Noodle.Fn.Updates (UpdateFocus(..))
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Raw.Node (Node(..), InletsValues, OutletsValues, NodeChanges) as Raw
import Noodle.Raw.Node as RawNode
import Noodle.Raw.Fn.Shape as RawShape
-- REM import Noodle.Stateful (get, getM, setM) as Stateful
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (id) as RawFamily
import Noodle.Repr (class DataFromToReprRow, class ToReprRow)
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Wiring (class Wiring)

import Cli.Keys (NodeBoxKey, PatchBoxKey, InletButtonKey, OutletButtonKey)
import Cli.Keys (mainScreen, patchBox, statusLine) as Key
import Cli.State (State) -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.State (LastKeys, nextKeys) as State -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.Style as Style

import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Cli.Palette.Item (crepr) as Palette
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (StatusLine, ChannelLabel, Documentation) as At
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)

-- REM import Cli.Components.Link as Link
-- REM import Cli.Components.NodeBox.InletsBox as InletsBox
-- REM import Cli.Components.NodeBox.OutletsBox as OutletsBox
-- REM import Cli.Components.NodeBox.InletButton as InletButton
-- REM import Cli.Components.NodeBox.OutletButton as OutletButton
-- REM import Cli.Components.NodeBox.RemoveButton as RemoveButton
-- REM import Cli.Components.NodeBox.InfoBox as InfoBox
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (cliSize, cliSizeRaw, renderCli, renderCliRaw)
import Cli.Class.CliRenderer (renderCli, renderCliRaw) as NodeBody
-- REM import Cli.Components.CommandLogBox as CommandLogBox
-- REM import Cli.Components.HydraCodeBox as HydraCodeBox
-- REM import Cli.Components.NodeBox.InfoBox as IB
-- REM import Cli.Components.StatusLine as SL
-- REM import Cli.Components.FullInfoBox as FI
import Cli.Bounds as Bounds


width :: Id.FamilyR -> Int -> Int -> Dimension
width familyName isCount osCount =
    Dimension.px $ widthN familyName isCount osCount


widthN :: Id.FamilyR -> Int -> Int -> Int
widthN familyName isCount osCount =
    20
    -- REM (max (String.length familyName) $ max (InletsBox.widthN isCount) (OutletsBox.widthN osCount)) + 4


-- widthN :: String -> Int -> Int -> Dimension


autoPos :: forall tk pstate fs repr m. BlessedOpM (State tk pstate fs repr m) m (Int /\ Int)
autoPos = do
    state <- State.get

    let topN = state.lastShift.x + 2
    let leftN = 16 + state.lastShift.y + 2
    pure (leftN /\ topN)


-- TODO: fromRawNodeAuto


fromNodeAuto
    :: forall tk fs pstate f nstate is os repr m
    -- REM . PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
    .  IsSymbol f => Mark (Id.Family f)
    => FromRepr repr nstate => ToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => CliFriendly tk fs repr m
    => Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.Family f
    -> Noodle.Node f nstate is os repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromNodeAuto curPatchId curPatch family node = do
    pos <- autoPos
    fromNodeAt pos curPatchId curPatch family node


-- TODO: fromRawNodeAuto

_component
    :: forall tk fs nstate pstate repr m
    .  Wiring m
    => HasFallback repr => Mark repr => T.At At.ChannelLabel repr
    => Int /\ Int
    -> Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.FamilyR
    -> Raw.Node nstate repr m
    -> State.LastKeys
    -> Maybe { width :: Int, height :: Int }
    -> BlessedOp nstate m
    -> BlessedOpM (State tk pstate fs repr m) m _
_component
    (leftN /\ topN)
    curPatchId
    curPatch
    familyR
    rawNode
    nextKeys
    mbBodySize
    nodeOp
    = do
    let (updates :: Signal (Raw.NodeChanges nstate repr)) = RawNode.subscribeChanges rawNode

    _ <- Blessed.lift $ RawNode._runOnInletUpdates rawNode
    _ <- Blessed.lift $ RawNode._runOnStateUpdates rawNode

    let top  = Offset.px topN
    let left = Offset.px leftN

    curChanges <- liftEffect $ RawNode.curChanges rawNode

    -- REM logNdfCommandM $ Cmd.MakeNode (Cmd.family $ reflect family) (Cmd.coord topN) (Cmd.coord $ leftN) (Cmd.nodeId $ reflect' nodeId) -- TODO: log somewhere else in a special place
    -- REM CommandLogBox.refresh

    let
        shape = RawNode.shape rawNode
        is = RawShape.inlets shape
        os = RawShape.outlets shape

    isValues <- RawNode.inlets rawNode  -- Sort using shape in the node?
    osValues <- RawNode.outlets rawNode -- Sort using shape in the node?

    {- REM
    let (nodeHolder :: Patch.HoldsNode Effect) = Patch.holdNode curPatch node
    let (toInletSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.InletR -> Maybe Hydra.WrapRepr)) = map R.getInletFromMap
    let (toOutletSignal :: Signal (R.NodeLineMap Hydra.WrapRepr) -> Signal (Id.OutletR -> Maybe Hydra.WrapRepr)) = map R.getOutletFromMap
    let isWithReprs = (\hiinr -> Node.withInletInNodeMRepr hiinr (\_ _ inletId -> Map.lookup (Id.inletR inletId) inletsReps) /\ hiinr) <$> is
    let osWithReprs = (\hoinr -> Node.withOutletInNodeMRepr hoinr (\_ _ outletId -> Map.lookup (Id.outletR outletId) outletsReprs) /\ hoinr) <$> os
    -}

    -- TODO: probably use Repr to create inlet bars and outlet bars, this way using Inlet' / Outlet' instances, we will probably be able to connect things
    --       or not Repr but some fold over inlets / outlets shape
    --       but the question remains: when we have some selected inlet for the receiving node in the handler, wherefrom do we get the node id of the outlet?
    --       we have the family encoded as symbol and hash of the is the thing that changes in real-time
    --       so we need to recreate the family. In case of Hydra, we have access to families' symbols but also by symbols.
    --       we have `lastClickedOtlet` in the state.
    --       Maybe try using `Exists` as we're sure the Node Family exists but don't want to parametrize `State` type with it.

    let
        (nodeIdR    :: Id.NodeR)   = RawNode.id rawNode
        (nodeFamily :: Id.FamilyR) = Id.familyOf nodeIdR
        boxWidth =
            case mbBodySize of
                Just { width } -> width - 1
                Nothing -> widthN nodeFamily (Array.length is) (Array.length os)
        boxHeight =
            case mbBodySize of
                Just { width, height } -> height + 2
                Nothing -> 5
        outletsTopOffset =
            Offset.px $
                case mbBodySize of
                    Just { height } -> height - 1
                    Nothing -> 2
        removeButtonOffset =
            Offset.px $
                case mbBodySize of
                    Just { height } -> height
                    Nothing -> 3
        {- REM
        inletsKeys /\ inletsBoxN =
            InletsBox.component curPatchId curPatch nextNodeBox nextInfoBox nextInletsBox family (toInletSignal updates') isWithReprs
        outletsKeys /\ outletsBoxN =
            OutletsBox.component outletsTopOffset nodeHolder nextNodeBox nextInfoBox nextOutletsBox (toOutletSignal updates') osWithReprs
        infoBoxN =
            InfoBox.component nextInfoBox $ boxWidth - 2
        removeButtonN =
            RemoveButton.component removeButtonOffset family node nextNodeBox nextInfoBox nextRemoveButton
        -}
        nextNodeBoxN =
            B.box nextKeys.nodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px boxWidth
                , Box.height $ Dimension.px boxHeight
                -- REM , Box.label $ T.singleLine $ T.nodeLabel family
                , Box.tags true
                , Style.nodeBoxBorder
                , Style.nodeBox
                -- REM , Core.on Element.Move
                -- REM     $ onMove nodeIdR nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
                -- REM , Core.on Element.MouseOver
                -- REM     $ onMouseOver family
                -- REM , Core.on Element.MouseOut
                -- REM     $ onMouseOut
                ]
                [ ]
        renderNodeUpdate :: forall a. Raw.NodeChanges nstate repr -> BlessedOp a m -- FIXME: shouldn't there be node state? but it's not used in the function anyway
        -- REM renderNodeUpdate = renderUpdate nextNodeBox inletsKeys outletsKeys
        renderNodeUpdate = renderUpdate nextKeys.nodeBox Map.empty Map.empty

    -- REM (stateRef :: Ref (State tk pstate fs repr m)) <- Blessed.getStateRef

    (nodeState :: nstate) <- RawNode.state rawNode

    Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM unit <<< renderNodeUpdate) -- FIXME: shouldn't there be node state? but it's not used in the function anyway
    -- REM liftEffect $ Signal.runSignal $ updates ~> logDataCommand stateRef

    -- REM liftEffect $ when (Lang.producesCode family) $ Signal.runSignal $ updates ~> updateCodeFor stateRef family

    _ <- Blessed.lift $ RawNode.run rawNode

    Key.patchBox >~ Node.append nextNodeBoxN

    {- REM
    nextNodeBox >~ Node.append inletsBoxN
    nextNodeBox >~ Node.append infoBoxN
    nextNodeBox >~ Node.append outletsBoxN
    nextNodeBox >~ Node.append removeButtonN
     -}

    -- REM? mapRepr2 <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node

    -- REM X liftEffect $ renderNodeUpdate $ Everything /\ nodeState /\ is /\ os

    (nodeStateRef :: Ref nstate) <- liftEffect $ Ref.new nodeState

    Blessed.lift $ Blessed.runM' nodeStateRef $ nodeOp

    let
        location =
            { left : leftN
            , top : topN
            , width : boxWidth
            , height : boxHeight
            }

    {- REM
    State.modify_ (_
        { lastShiftX = state.lastShiftX + 1
        , lastShiftY = state.lastShiftY + 1
        , lastKeys = nextKeys
        , nodeKeysMap = Map.insert nodeIdR nextNodeBox state.nodeKeysMap
        , locations   = Map.insert nodeIdR location state.locations
        }
    )
    -}

    -- REM Key.mainScreen >~ Screen.render

    pure unit -- REM { nextNodeBoxN, inletsBoxN, outletsBoxN, nextNodeBox }



fromRawNodeAt
    :: forall tk fs nstate pstate repr m
    -- => IsSymbol f => Mark (Id.Family f)
    -- => HasFallback repr => Mark repr => T.At At.ChannelLabel repr
     . CliFriendly tk fs repr m
    => Int /\ Int
    -> Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.FamilyR
    -> Raw.Node nstate repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromRawNodeAt pos curPatchId curPatch familyR rawNode = do
    --liftEffect $ Node.run node -- just Node.run ??
    state <- State.get
    let
        nextKeys = State.nextKeys state.lastKeys
        mbSize = cliSizeRaw   (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
        nodeOp = renderCliRaw (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
    _component pos curPatchId curPatch familyR rawNode nextKeys mbSize nodeOp


fromNodeAt
    :: forall tk fs pstate f nstate is os repr m
    .  Wiring m
    => IsSymbol f => Mark (Id.Family f)
    => FromRepr repr nstate => ToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => CliFriendly tk fs repr m
    -- => HasCliCustomSize tk f (Noodle.Node f nstate is os repr m)
    => Int /\ Int
    -> Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.Family f
    -> Noodle.Node f nstate is os repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromNodeAt pos curPatchId curPatch family node = do
    state <- State.get
    let
        familyR = Id.familyR family
        rawNode = Node.toRaw node
        nextKeys = State.nextKeys state.lastKeys
        mbSize = cliSize   (Proxy :: _ tk) (Proxy :: _ fs) family nextKeys.nodeBox node
        nodeOp = renderCli (Proxy :: _ tk) (Proxy :: _ fs) family nextKeys.nodeBox node
    _component pos curPatchId curPatch familyR rawNode nextKeys mbSize nodeOp


fromFamilyAt
    :: forall tk fs pstate f nstate is os repr m
     . IsSymbol f => Mark (Id.Family f)
    => FromRepr repr nstate => ToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => CliFriendly tk fs repr m
    => Int /\ Int
    -> Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.Family f
    -- REM -> Raw.Family repr m -- TODO: implement Raw version as well
    -> Toolkit tk fs repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromFamilyAt pos curPatchId curPatch family tk = do
    (node :: Noodle.Node f nstate is os repr m) <- Blessed.lift' $ Toolkit.spawn family tk
    {- REM
    let (mbState :: Maybe pstate) = fromGlobal $ Stateful.get curPatch
    node' <- liftEffect $ case mbState of
        Just state -> Stateful.setM state node
        Nothing -> pure node
    fromNodeAt pos curPatchId curPatch family node'
     -}
    -- TODO: update node in the patch?
    fromNodeAt pos curPatchId curPatch family node


fromFamilyAuto
    :: forall fs tk f nstate pstate is os repr m
     . IsSymbol f => Mark (Id.Family f)
    => FromRepr repr nstate => ToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => CliFriendly tk fs repr m
    => Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Id.Family f
    -> Toolkit tk fs repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromFamilyAuto curPatchId curPatch family tk = do
    pos <- autoPos
    (node :: Noodle.Node f nstate is os repr m) <- Blessed.lift' $ Toolkit.spawn family tk
    fromNodeAt pos curPatchId curPatch family node


fromRawFamilyAuto
    :: forall tk fs pstate repr m
    .  Wiring m
    => HasFallback repr => Mark repr => T.At At.ChannelLabel repr
    -- => IsSymbol f => Mark (Id.Family f)
    -- => HasFallback repr => Mark repr => T.At At.ChannelLabel repr
    => CliFriendly tk fs repr m
    => Id.PatchR
    -> Noodle.Patch pstate fs repr m
    -> Raw.Family repr repr m
    -> Toolkit tk fs repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
fromRawFamilyAuto curPatchId curPatch rawFamily tk = do
    pos <- autoPos
    let familyR = RawFamily.id rawFamily
    (mbNode :: Maybe (Raw.Node repr repr m)) <- Blessed.lift' $ Toolkit.spawnAnyRaw familyR tk
    case mbNode of
        Just node -> fromRawNodeAt pos curPatchId curPatch familyR node
        Nothing -> pure unit


logDataCommand
    :: forall tk fs pstate nstate repr m
     . MonadEffect m
    => Ref (State tk pstate fs repr m)
    -> Raw.NodeChanges nstate repr
    -> m Unit
logDataCommand stateRef (chFocus /\ _ /\ inlets /\ outlets) =
    case chFocus of
        InletUpdate inletR ->
            case Map.lookup inletR inlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.Send (Cmd.nodeId $ reflect' nodeId) (Cmd.inletAlias $ reflect' inlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        OutletUpdate outletR ->
            case Map.lookup outletR outlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.SendO (Cmd.nodeId $ reflect' nodeId) (Cmd.outletAlias $ reflect' outlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        _ -> pure unit


updateCodeFor
    :: forall tk f s fs nstate repr m
     . MonadEffect m
    => IsSymbol f
    => Ref (State tk s fs repr m)
    -> Id.Family f
    -> Raw.NodeChanges nstate repr
    -> m Unit
updateCodeFor stateRef family update@(_ /\ nodeId /\ _) = do
    {- REM
    flip (logLangCommandByRef nodeId) stateRef $ Lang.updateToCommand family $ Tuple.snd update
    liftEffect $ Blessed.runM' stateRef HydraCodeBox.refresh -- FIXME: use `Blessed.impairN`
    state <- liftEffect $ Ref.read stateRef
    case state.wsServer of
        Just serverState ->
            liftEffect $ flip WSS.broadcastProgram serverState $ Lang.formProgram state.program
        Nothing -> pure unit
    -}
    pure unit


renderUpdate
    :: forall m nstate state repr
     . Mark repr
    => T.At At.ChannelLabel repr
    => NodeBoxKey
    -> Map Id.InletR  InletButtonKey
    -> Map Id.OutletR OutletButtonKey
    -> Raw.NodeChanges nstate repr
    -> BlessedOp state m
renderUpdate _ inletsKeysMap outletsKeysMap (_ /\ stateRepr /\ inletsReps /\ outletsReprs) = do
    -- liftEffect $ Console.log $ show outletsReprs
    _ <- traverseWithIndex updateInlet inletsReps
    _ <- traverseWithIndex updateOutlet outletsReprs
    Key.mainScreen >~ Screen.render
    where
        updateInlet inletR repr =
            case Map.lookup inletR inletsKeysMap of
                Just inletKey -> do
                    inletKey >~ Box.setContent $ T.singleLine $ T.inlet' 0 inletR $ Just repr
                Nothing -> pure unit
        updateOutlet outletR repr =
            case Map.lookup outletR outletsKeysMap of
                Just outletKey -> do
                    outletKey >~ Box.setContent $ T.singleLine $ T.outlet' 0 outletR $ Just repr
                Nothing -> pure unit


onMove :: forall tk pstate fs repr m. Id.NodeR -> NodeBoxKey -> NodeBoxKey -> EventJson -> BlessedOp (State tk pstate fs repr m) Effect
onMove nodeId nodeKey _ _ = do
    let rawNk = NodeKey.rawify nodeKey
    newBounds <- Bounds.collect nodeId nodeKey
    state <- State.modify \s -> s { locations = Map.update (updatePos newBounds) nodeId s.locations }
    {- REM
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) Link.update
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) Link.update
    -}
    pure unit
    where
        updatePos nb = Just <<< Bounds.move nb


onMouseOver :: forall tk fs f pstate repr m. IsSymbol f => Id.Family f -> _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOver family _ _ = do
    -- maybeRepr <- liftEffect $ Signal.get reprSignal
    -- -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inletId
    {- REM
    SL.familyStatus family
    FI.familyStatus family
     -}
    Key.mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: forall tk fs pstate repr m. _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOut _ _ = do
    {- REM
    SL.clear
    FI.clear
    -}
    Key.mainScreen >~ Screen.render