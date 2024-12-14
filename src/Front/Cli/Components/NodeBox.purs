module Cli.Components.NodeBox where

import Prelude

import Data.Text.Output.Blessed (singleLine) as T

import Cli.WsServer as WSS
import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (Error)

import Control.Monad.State (get, modify, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Foldable (for_)
-- REM import Data.KeyHolder as KH
import Data.Symbol (class IsSymbol)
import Data.String as String
import Data.TraversableWithIndex (traverseWithIndex)
-- REM import Data.SProxy (reflect, reflect')
-- REM import Data.FromToFile (class Encode, encode)

import Signal (Signal, (~>))
import Signal.Extra as SignalX

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp (lift, runM, runM') as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent)  as Box
-- import Blessed.UI.Line.Li ()

import Noodle.Repr.ChRepr (class HasFallback, class FromRepr, class ToRepr)
import Noodle.Id as Id
import Noodle.Toolkit (class MarkToolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Node (toRaw) as Node
import Noodle.Node (Node) as Noodle
import Noodle.Fn.Updates (UpdateFocus(..))
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Raw.Node as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Wiring (class Wiring)
import Noodle.Fn.ToFn (class PossiblyToFn)

import Noodle.Ui.Cli.Tagging (inlet, nodeLabel, outlet) as T
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (StatusLine, ChannelLabel, Documentation) as At

import Cli.Keys (NodeBoxKey, InletButtonKey, OutletButtonKey)
import Cli.Keys (mainScreen, patchBox) as Key
import Cli.State (State) -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.State (LastKeys, nextKeys) as State -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.Style as Style

import Cli.Components.Link as CLink
import Cli.Components.NodeBox.InletsBox as InletsBox
import Cli.Components.NodeBox.OutletsBox as OutletsBox
import Cli.Components.NodeBox.InfoBox as InfoBox
-- REM import Cli.Components.NodeBox.InletButton as InletButton
-- REM import Cli.Components.NodeBox.OutletButton as OutletButton
-- REM import Cli.Components.NodeBox.RemoveButton as RemoveButton
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (cliSizeRaw, renderCliRaw)
-- REM import Cli.Components.CommandLogBox as CommandLogBox
-- REM import Cli.Components.HydraCodeBox as HydraCodeBox
-- REM import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
-- REM import Cli.Components.FullInfoBox as FI
import Cli.Bounds as Bounds


width :: Id.FamilyR -> Int -> Int -> Dimension
width familyName isCount osCount =
    Dimension.px $ widthN familyName isCount osCount


widthN :: Id.FamilyR -> Int -> Int -> Int
widthN familyName isCount osCount =
    (max (String.length $ Id.family familyName) $ max (InletsBox.widthN isCount) (OutletsBox.widthN osCount)) + 4


-- widthN :: String -> Int -> Int -> Dimension


nextPos :: { left :: Int, top :: Int } -> { left :: Int, top :: Int }
nextPos { left, top } =
    { left : 16 + left + 2
    , top : top + 2
    }


_component
    :: forall tk fs fstate pstate repr m
    .  Wiring m
    => HasFallback chrepr
    => PossiblyToFn tk (Maybe chrepr) (Maybe chrepr) Id.FamilyR
    => CliFriendly tk fs repr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.FamilyR
    -> Raw.Node fstate repr m
    -> State.LastKeys
    -> Maybe { width :: Int, height :: Int }
    -> BlessedOp fstate m
    -> BlessedOpM (State tk pstate fs repr m) m _
_component
    pos
    patchR
    familyR
    rawNode
    keys
    mbBodySize
    nodeOp
    = do
    let (updates :: Signal (Raw.NodeChanges fstate repr)) = RawNode.subscribeChanges rawNode

    _ <- Blessed.lift $ RawNode._runOnInletUpdates rawNode
    _ <- Blessed.lift $ RawNode._runOnStateUpdates rawNode

    let top  = Offset.px pos.top
    let left = Offset.px pos.left

    curChanges <- liftEffect $ RawNode.curChanges rawNode

    -- REM logNdfCommandM $ Cmd.MakeNode (Cmd.family $ reflect family) (Cmd.coord topN) (Cmd.coord $ leftN) (Cmd.nodeId $ reflect' nodeId) -- TODO: log somewhere else in a special place
    -- REM CommandLogBox.refresh

    let
        shape = RawNode.shape rawNode
        is = RawShape.inlets shape
        os = RawShape.outlets shape

    isValues <- RawNode.inlets rawNode  -- Sort using shape in the node?
    osValues <- RawNode.outlets rawNode -- Sort using shape in the node?

    -- TODO: probably use Repr to create inlet bars and outlet bars, this way using Inlet' / Outlet' instances, we will probably be able to connect things
    --       or not Repr but some fold over inlets / outlets shape
    --       but the question remains: when we have some selected inlet for the receiving node in the handler, wherefrom do we get the node id of the outlet?
    --       we have the family encoded as symbol and hash of the is the thing that changes in real-time
    --       so we need to recreate the family. In case of Hydra, we have access to families' symbols but also by symbols.
    --       we have `lastClickedOtlet` in the state.
    --       Maybe try using `Exists` as we're sure the Node Family exists but don't want to parametrize `State` type with it.

    let
        (nodeR   :: Id.NodeR)   = RawNode.id rawNode
        (familyR :: Id.FamilyR) = Id.familyOf nodeR
        boxWidth =
            case mbBodySize of
                Just { width } -> width - 1
                Nothing -> widthN familyR (Array.length is) (Array.length os)
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
        inletsKeys /\ inletsBoxN =
            InletsBox.component patchR keys familyR nodeR (updates ~> _.inlets) $ RawNode.orderInlets shape isValues
        outletsKeys /\ outletsBoxN =
            OutletsBox.component outletsTopOffset keys familyR nodeR (updates ~> _.outlets) $ RawNode.orderOutlets shape osValues
        infoBoxN =
            InfoBox.component keys.infoBox $ boxWidth - 2
        {- REM
        removeButtonN =
            RemoveButton.component removeButtonOffset family node nextNodeBox nextInfoBox nextRemoveButton
        -}
        nextNodeBoxN =
            B.box keys.nodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px boxWidth
                , Box.height $ Dimension.px boxHeight
                , Box.label $ T.singleLine $ T.nodeLabel (Proxy :: _ tk ) familyR
                , Box.tags true
                , Style.nodeBoxBorder
                , Style.nodeBox
                , Core.on Element.Move
                    $ onMove nodeR keys.nodeBox
                , Core.on Element.MouseOver
                    $ onMouseOver (Proxy :: _ tk) familyR
                , Core.on Element.MouseOut
                   $ onMouseOut
                ]
                [ ]
        renderNodeUpdate :: forall a. Raw.NodeChanges fstate repr -> BlessedOp a m -- FIXME: shouldn't there be node state? but it's not used in the function anyway
        renderNodeUpdate = renderUpdate keys.nodeBox inletsKeys outletsKeys

    -- REM (stateRef :: Ref (State tk pstate fs repr m)) <- Blessed.getStateRef

    (nodeState :: fstate) <- RawNode.state rawNode

    Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM unit <<< renderNodeUpdate) -- FIXME: shouldn't there be node state? but it's not used in the function anyway
    -- REM liftEffect $ Signal.runSignal $ updates ~> logDataCommand stateRef

    -- REM liftEffect $ when (Lang.producesCode family) $ Signal.runSignal $ updates ~> updateCodeFor stateRef family

    _ <- Blessed.lift $ RawNode.run rawNode

    Key.patchBox >~ Node.append nextNodeBoxN

    keys.nodeBox >~ Node.append inletsBoxN
    keys.nodeBox >~ Node.append outletsBoxN
    keys.nodeBox >~ Node.append infoBoxN
    {- REM
    nextNodeBox >~ Node.append removeButtonN
     -}

    -- REM? mapRepr2 <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node

    -- REM X liftEffect $ renderNodeUpdate $ Everything /\ nodeState /\ is /\ os

    (nodeStateRef :: Ref fstate) <- liftEffect $ Ref.new nodeState

    Blessed.lift $ Blessed.runM' nodeStateRef $ nodeOp

    let
        location =
            { left : pos.left
            , top : pos.top
            , width : boxWidth
            , height : boxHeight
            }

    State.modify_ (\s -> s
        { lastShift = -- FIXME: should record last location
            { left : s.lastShift.left + 1
            , top : s.lastShift.top + 1
            }
        , nodeKeysMap = Map.insert nodeR keys.nodeBox s.nodeKeysMap
        , locations   = Map.insert nodeR location s.locations
        , lastKeys = keys
        }
    )

    Key.mainScreen >~ Screen.render

    pure unit -- REM { nextNodeBoxN, inletsBoxN, outletsBoxN, nextNodeBox }



componentRaw
    :: forall tk fs fstate pstate repr m
     . Wiring m
    => HasFallback chrepr
    => PossiblyToFn tk (Maybe chrepr) (Maybe chrepr) Id.FamilyR
    => CliFriendly tk fs repr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.FamilyR
    -> Raw.Node fstate repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
componentRaw pos curPatchR familyR rawNode = do
    -- REM liftEffect $ Node.run node -- just Node.run ??
    state <- State.get
    let
        nextKeys = State.nextKeys state.lastKeys
        mbSize = cliSizeRaw   (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
        nodeOp = renderCliRaw (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
    _component pos curPatchR familyR rawNode nextKeys mbSize nodeOp


component
    :: forall tk fs pstate f fstate is os repr m
    .  Wiring m
    => IsSymbol f
    => FromRepr repr fstate => ToRepr fstate repr
    => RegisteredFamily (F f fstate is os repr m) fs
    => PossiblyToFn tk (Maybe chrepr) (Maybe chrepr) Id.FamilyR
    => CliFriendly tk fs repr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.Family f
    -> Noodle.Node f fstate is os repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
component pos curPatchR family =
    componentRaw pos curPatchR (Id.familyR family) <<< Node.toRaw


renderUpdate
    :: forall m fstate state repr
     . T.At At.ChannelLabel repr
    => NodeBoxKey
    -> Map Id.InletR  InletButtonKey
    -> Map Id.OutletR OutletButtonKey
    -> Raw.NodeChanges fstate repr
    -> BlessedOp state m
renderUpdate _ inletsKeysMap outletsKeysMap update = do
    -- CC.log $ show outletsReprs
    _ <- traverseWithIndex updateInlet update.inlets
    _ <- traverseWithIndex updateOutlet update.outlets
    Key.mainScreen >~ Screen.render
    where
        updateInlet inletR repr =
            case Map.lookup inletR inletsKeysMap of
                Just inletKey -> do
                    inletKey >~ Box.setContent $ T.singleLine $ T.inlet 0 inletR $ Just repr
                Nothing -> pure unit
        updateOutlet outletR repr =
            case Map.lookup outletR outletsKeysMap of
                Just outletKey -> do
                    outletKey >~ Box.setContent $ T.singleLine $ T.outlet 0 outletR $ Just repr
                Nothing -> pure unit


updateCodeFor
    :: forall tk f s fs fstate repr m
     . MonadEffect m
    => IsSymbol f
    => Ref (State tk s fs repr m)
    -> Id.Family f
    -> Raw.NodeChanges fstate repr
    -> m Unit
updateCodeFor stateRef family update = do
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


logDataCommand
    :: forall tk fs pstate fstate repr m
     . MonadEffect m
    => Ref (State tk pstate fs repr m)
    -> Raw.NodeChanges fstate repr
    -> m Unit
logDataCommand stateRef update =
    case update.focus of
        InletUpdate inletR ->
            case Map.lookup inletR update.inlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.Send (Cmd.nodeId $ reflect' nodeId) (Cmd.inletAlias $ reflect' inlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        OutletUpdate outletR ->
            case Map.lookup outletR update.outlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.SendO (Cmd.nodeId $ reflect' nodeId) (Cmd.outletAlias $ reflect' outlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        _ -> pure unit


onMove :: forall tk pstate fs repr m. Id.NodeR -> NodeBoxKey -> NodeBoxKey -> EventJson -> BlessedOp (State tk pstate fs repr m) Effect
onMove nodeId nodeKey _ _ = do
    let rawNk = NodeKey.toRaw nodeKey
    newBounds <- Bounds.collect nodeId nodeKey
    state <- State.modify \s -> s { locations = Map.update (updatePos newBounds) nodeId s.locations }
    CLink.runB $ do
        for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) CLink.update
        for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) CLink.update
    pure unit
    where
        updatePos nb = Just <<< Bounds.move nb


onMouseOver
    :: forall tk pstate fs repr m
     . MarkToolkit tk
    => Toolkit.HasRepr tk repr
    => T.At At.StatusLine repr
    => PossiblyToFn tk (Maybe chrepr) (Maybe chrepr) Id.FamilyR
    => Proxy tk
    -> Id.FamilyR
    -> _
    -> _
    -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOver ptk familyR _ _ = do
    -- maybeRepr <- liftEffect $ Signal.get reprSignal
    -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inletId

    SL.familyStatus ptk familyR
    {-
    FI.familyStatus family
     -}
    Key.mainScreen >~ Screen.render
    --CC.log $ "over" <> show idx


onMouseOut :: forall tk fs pstate repr m. _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOut _ _ = do
    SL.clear
    {- REM
    FI.clear
    -}
    Key.mainScreen >~ Screen.render