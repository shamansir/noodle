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
import Effect.Console (log) as Console

import Control.Monad.State (get, modify, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
import Data.Array as Array
import Data.Foldable (for_, traverse_)
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

import Blessed.Core.Coord ((<+>))
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp (getStateRef, lift, runM, runM', runOnUnit, runOn, imapStateF) as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent)  as Box
-- import Blessed.UI.Line.Li ()

import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Id as Id
import Noodle.Toolkit (class MarkToolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Node (toRaw) as Node
import Noodle.Node (Node) as Noodle
import Noodle.Toolkit.Families (F, class RegisteredFamily)
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Raw.Node as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Raw.Link as Link
import Noodle.Wiring (class Wiring)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Repr.Tagged (class Tagged) as CT

 -- FIXME: `Repr.Tag` and `Format.Tag` are very different things, find a way to make it clear (name it `Style`?)
import Noodle.Ui.Cli.Tagging (inlet, nodeLabel, outlet) as T
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (StatusLine, ChannelLabel, Documentation) as At

import Cli.Keys (NodeBoxKey, InletButtonKey, OutletButtonKey)
import Cli.Keys (mainScreen, patchBox) as Key
import Cli.State (State) -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.State (LastKeys, nextKeys, storeNodeUpdate, lastNodeUpdate) as State -- REM , logNdfCommandM, logNdfCommandByRef, logLangCommandByRef)
import Cli.State (Focus(..)) as Focus
import Cli.Style as Style
import Cli.Bounds as Bounds

import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (cliSize, cliSizeRaw, renderCli, renderCliRaw)

import Cli.Components.Link as CLink
import Cli.Components.NodeBox.InletsBox as InletsBox
import Cli.Components.NodeBox.OutletsBox as OutletsBox
import Cli.Components.NodeBox.InfoBox as InfoBox
-- REM import Cli.Components.NodeBox.InletButton as InletButton
-- REM import Cli.Components.NodeBox.OutletButton as OutletButton
import Cli.Components.NodeBox.RemoveButton as RemoveButton
-- REM import Cli.Components.CommandLogBox as CommandLogBox
-- REM import Cli.Components.HydraCodeBox as HydraCodeBox
-- REM import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
-- REM import Cli.Components.FullInfoBox as FI
-- import Cli.Components.SidePanel.Console as CC
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Documentation as DP
import Cli.Components.NodeBox.InletIndicator as II
import Cli.Components.NodeBox.OutletIndicator as OI


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
    :: forall tk fs pstate strepr chrepr m
    .  Wiring m
    => HasFallback chrepr
    => CT.Tagged chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.FamilyR
    -> Raw.Node strepr chrepr m
    -> State.LastKeys
    -> Maybe { width :: Int, height :: Int }
    -> Maybe (BlessedOp strepr m)
    -> BlessedOpM (State tk pstate fs strepr chrepr m) m _
_component
    pos
    patchR
    familyR
    rawNode
    keys
    mbBodySize
    mbNodeOp
    = do
    let (updates :: Signal (Raw.NodeChanges strepr chrepr)) = RawNode.subscribeChanges rawNode

    _ <- Blessed.lift $ RawNode._runOnInletUpdates rawNode

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

    stateRef <- Blessed.getStateRef

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
            InletsBox.component stateRef patchR keys rawNode (updates ~> _.inlets) $ RawNode.orderInlets shape isValues
        outletsKeys /\ outletsBoxN =
            OutletsBox.component outletsTopOffset keys patchR nodeR (updates ~> _.outlets) $ RawNode.orderOutlets shape osValues
        infoBoxN =
            InfoBox.component keys.infoBox $ boxWidth - 2
        removeButtonN =
            RemoveButton.component removeButtonOffset familyR rawNode keys
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
                    $ onMouseOver (Proxy :: _ tk) patchR nodeR
                , Core.on Element.MouseOut
                   $ onMouseOut
                ]
                [ ]
        renderNodeUpdate :: Raw.NodeChanges strepr chrepr -> BlessedOp (State tk pstate fs strepr chrepr m) m -- FIXME: shouldn't there be node state? but it's not used in the function anyway
        renderNodeUpdate = renderUpdate keys.nodeBox (Proxy :: _ tk) nodeR inletsKeys outletsKeys

    -- REM (stateRef :: Ref (State tk pstate fs repr m)) <- Blessed.getStateRef

    -- state <- State.get
    (nodeState :: strepr) <- RawNode.state rawNode

    stateRef <- Blessed.getStateRef

    -- Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM state <<< CC.log <<< ?wh)
    Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM' stateRef <<< storeNodeUpdate nodeR)
    Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM' stateRef <<< renderNodeUpdate) -- FIXME: shouldn't there be node state? but it's not used in the function anyway
    -- REM Blessed.lift $ SignalX.runSignal $ updates ~> (Blessed.runM' stateRef <<< logUpdateToConsole) -- FIXME: shouldn't there be node state? but it's not used in the function anyway
    -- REM Blessed.lift $ SignalX.runSignal $ updates ~> logDataCommand stateRef -- TODO: only inlude changes from node editors and node body

    -- REM liftEffect $ when (Lang.producesCode family) $ Signal.runSignal $ updates ~> updateCodeFor stateRef family

    _ <- Blessed.lift $ RawNode.run rawNode

    Key.patchBox >~ Node.append nextNodeBoxN

    keys.nodeBox >~ Node.append inletsBoxN
    keys.nodeBox >~ Node.append outletsBoxN
    keys.nodeBox >~ Node.append infoBoxN
    keys.nodeBox >~ Node.append removeButtonN
    when (isJust mbNodeOp)
        $ let
            nextBodyOverlayN =
                B.box keys.bodyOverlay
                    [ Box.draggable false
                    , Box.top $ Offset.px 1
                    , Box.left $ Offset.px 0
                    , Box.width $ Dimension.px $ boxWidth - 2
                    , Box.height $ Dimension.px $ boxHeight - 4
                    , Style.bodyOverlay
                    ]
                    [ ]
        in keys.nodeBox >~ Node.append nextBodyOverlayN

    -- REM? mapRepr2 <- liftEffect $ R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node

    -- REM X liftEffect $ renderNodeUpdate $ Everything /\ nodeState /\ is /\ os

    (nodeStateRef :: Ref strepr) <- liftEffect $ Ref.new nodeState

    -- liftEffect $ Console.log "before render cli"
    -- liftEffect $ Console.log "test second line"
    case mbNodeOp of
        Just nodeOp -> Blessed.lift $ Blessed.runM' nodeStateRef nodeOp
        Nothing -> pure unit

    let
        location =
            { left : pos.left + 1
            , top : pos.top + 1
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

    pure unit -- REM { nextNodeBoxN, inletsBoxN, outletsBoxN, nextNodeBox }



componentRaw
    :: forall tk fs pstate strepr chrepr m
     . Wiring m
    => HasFallback chrepr
    => CT.Tagged chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.FamilyR
    -> Raw.Node strepr chrepr m
    -> BlessedOpM (State tk pstate fs strepr chrepr m) m _
componentRaw pos curPatchR familyR rawNode = do
    -- REM liftEffect $ Node.run node -- just Node.run ??
    state <- State.get
    let
        nextKeys = State.nextKeys state.lastKeys
        mbSize = cliSizeRaw   (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
        nodeOp = renderCliRaw (Proxy :: _ tk) (Proxy :: _ fs) familyR nextKeys.nodeBox rawNode
    _component pos curPatchR familyR rawNode nextKeys mbSize nodeOp


component
    :: forall tk fs pstate f fstate is os strepr chrepr m
    .  Wiring m
    => IsSymbol f
    => HasFallback chrepr
    => HasFallback fstate
    => CT.Tagged chrepr
    => StRepr fstate strepr
    => RegisteredFamily (F f fstate is os chrepr m) fs
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr m
    => { left :: Int, top :: Int }
    -> Id.PatchR
    -> Id.Family f
    -> Noodle.Node f fstate is os chrepr m
    -> BlessedOpM (State tk pstate fs strepr chrepr m) m _
component pos curPatchR family node = do
    state <- State.get
    let
        nextKeys = State.nextKeys state.lastKeys
        familyR = Id.familyR family
        mbSize = cliSize   (Proxy :: _ tk) (Proxy :: _ fs) family nextKeys.nodeBox node
        mbNodeOp = renderCli (Proxy :: _ tk) (Proxy :: _ fs) family nextKeys.nodeBox node
        rawNode = RawNode.toReprableState $ Node.toRaw node
    _component pos curPatchR familyR rawNode nextKeys mbSize (Blessed.runOn fallback <$> mbNodeOp)


storeNodeUpdate
    :: forall tk fs pstate strepr chrepr m
     . Id.NodeR
    -> Raw.NodeChanges strepr chrepr
    -> BlessedOp (State tk pstate fs strepr chrepr m) m
storeNodeUpdate nodeR =
    State.modify_ <<< State.storeNodeUpdate nodeR


renderUpdate
    :: forall tk fs pstate strepr chrepr m
     . MonadEffect m
    => MarkToolkit tk
    => Toolkit.HasChRepr tk chrepr
    => T.At At.StatusLine chrepr
    => T.At At.ChannelLabel chrepr
    => T.At At.Documentation chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => NodeBoxKey
    -> Proxy tk
    -> Id.NodeR
    -> Map Id.InletR  InletButtonKey
    -> Map Id.OutletR OutletButtonKey
    -> Raw.NodeChanges strepr chrepr
    -> BlessedOp (State tk pstate fs strepr chrepr m) m
renderUpdate _ ptk nodeR inletsKeysMap outletsKeysMap update = do
    -- CC.log $ show outletsReprs
    _ <- traverseWithIndex updateInlet  update.inlets
    _ <- traverseWithIndex updateOutlet update.outlets
    state <- State.get
    case state.mouseOverFocus of
        Just (Focus.Node _ moNodeId) ->
            when (nodeR == moNodeId) $ do
                SL.nodeStatus ptk nodeR update
                DP.showNodeDocumentation nodeR $ Just update
        Just (Focus.Inlet _ moNodeR moInletR) ->
            when (nodeR == moNodeR) $
                traverseWithIndex (updateStatusLineAsInlet moInletR) update.inlets >>= mempty
        Just (Focus.Outlet _ moNodeR moOutletR) ->
            when (nodeR == moNodeR) $
                traverseWithIndex (updateStatusLineAsOutlet moOutletR) update.outlets >>= mempty
        Just (Focus.Patch _) ->
            pure unit
        Nothing -> pure unit
    Key.mainScreen >~ Screen.render
    where
        updateInlet (iidx /\ inletR) vicRepr =
            case Map.lookup inletR inletsKeysMap of
                Just inletKey -> do
                    inletKey >~ Box.setContent $ T.singleLine $ T.inlet iidx inletR vicRepr
                Nothing -> pure unit
        updateOutlet (oidx /\ outletR) vicRepr =
            case Map.lookup outletR outletsKeysMap of
                Just outletKey -> do
                    outletKey >~ Box.setContent $ T.singleLine $ T.outlet oidx outletR vicRepr
                Nothing -> pure unit
        updateStatusLineAsInlet moInletR (iidx /\ inletR) vicRepr =
            when (moInletR == inletR) $
                SL.inletStatus (Id.familyOf nodeR) iidx inletR vicRepr
        updateStatusLineAsOutlet moOutletR (oidx /\ outletR) vicRepr =
            when (moOutletR == outletR) $
                SL.outletStatus (Id.familyOf nodeR) oidx outletR vicRepr


updateCodeFor
    :: forall f fstate tk pstate fs strepr chrepr m
     . MonadEffect m
    => IsSymbol f
    => Ref (State tk pstate fs strepr chrepr m)
    -> Id.Family f
    -> Raw.NodeChanges fstate chrepr
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


{- REM
logUpdateToConsole :: forall tk fs pstate fstate strepr chrepr m
     . MonadEffect m
    => Show fstate
    => Show chrepr
    => Raw.NodeChanges fstate chrepr
    -> BlessedOpM (State tk pstate fs strepr chrepr m) m _
logUpdateToConsole updates =
    CC.log $ show updates
-}



{- REM
logDataCommand
    :: forall tk fs pstate fstate strepr chrepr m
     . MonadEffect m
    => Ref (State tk pstate fs strepr chrepr m)
    -> Raw.NodeChanges fstate chrepr
    -> m Unit
logDataCommand stateRef update =
    case update.focus of
        InletUpdate inletR ->
            case Map.lookup inletR $ Map.mapKeys Tuple.snd update.inlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.Send (Cmd.nodeId $ reflect' nodeId) (Cmd.inletAlias $ reflect' inlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        OutletUpdate outletR ->
            case Map.lookup outletR $ Map.mapKeys Tuple.snd update.outlets of
                Just wrapRepr -> do
                    -- REM flip logNdfCommandByRef stateRef $ Cmd.SendO (Cmd.nodeId $ reflect' nodeId) (Cmd.outletAlias $ reflect' outlet) $ Cmd.encodedValue $ encode wrapRepr
                    -- REM liftEffect $ Blessed.runM' stateRef CommandLogBox.refresh -- FIXME: use `Blessed.impairN`
                    pure unit
                Nothing -> pure unit
        _ -> pure unit
-}


onMove :: forall tk ps fs sr cr m. Id.NodeR -> NodeBoxKey -> NodeBoxKey -> EventJson -> BlessedOp (State tk ps fs sr cr m) Effect
onMove nodeR nodeKey _ _ = do
    II.hide
    OI.hide
    -- let rawNk = NodeKey.toRaw nodeKey
    newBounds <- Bounds.collect nodeR nodeKey
    state <- State.modify \s -> s { locations = Map.update (updatePos newBounds) nodeR s.locations }
    Blessed.runOnUnit $ do
        traverse_ CLink.update $ Map.filter (CLink.of_ >>> Id.connectedTo nodeR) state.links
        -- for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) CLink.update
        -- for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) CLink.update
    CL.trackCommand $ QOp.moveNode nodeR { left : newBounds.left, top : newBounds.top }
    where
        updatePos nb = Just <<< Bounds.move nb


onMouseOver
    :: forall tk pstate fs strepr chrepr m
     . MarkToolkit tk
    => Toolkit.HasChRepr tk chrepr
    => T.At At.StatusLine chrepr
    => T.At At.Documentation chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Proxy tk
    -> Id.PatchR
    -> Id.NodeR
    -> _
    -> _
    -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onMouseOver ptk patchR nodeR _ _ = do
    -- maybeRepr <- liftEffect $ Signal.get reprSignal
    -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inletId
    state <- State.get
    case State.lastNodeUpdate nodeR state of
        Just update -> do
            SL.nodeStatus ptk nodeR update
            DP.showNodeDocumentation nodeR $ Just update
        Nothing -> do
            SL.familyStatus ptk $ Id.familyOf nodeR
            DP.showNodeDocumentation nodeR Nothing
    State.modify_ $ _ { mouseOverFocus = Just $ Focus.Node patchR nodeR }
    {-
    FI.familyStatus family
     -}
    Key.mainScreen >~ Screen.render
    --CC.log $ "over" <> show idx


onMouseOut
    :: forall tk ps fs sr cr m
     . MarkToolkit tk
    => Toolkit.HasChRepr tk cr
    => T.At At.StatusLine cr
    => T.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => _ -> _ -> BlessedOp (State tk ps fs sr cr m) Effect
onMouseOut _ _ = do
    State.modify_ $ _ { mouseOverFocus = Nothing }
    SL.clear
    -- DP.clear
    {- REM
    FI.clear
    -}
    Key.mainScreen >~ Screen.render