module Web.Components.AppScreen.State where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set (empty, fromFoldable) as Set
import Data.Map (Map)
import Data.Map (empty, insert, lookup, delete) as Map
import Data.Map.Extra (update', lookupKey) as Map
import Data.FunctorWithIndex (mapWithIndex)
import Data.Traversable (traverse)
import Data.Text.Format (Tag) as T
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (length, snoc, index, findIndex) as Array
import Data.UniqueHash (UniqueHash)

import Noodle.Id (PatchR, NodeR, FamilyR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class InitPatchState, initPatch, class HoldsFamilies, families) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make, getState, registerRawNode, nodesCount, linksCount, allNodes) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, patch, addPatch, withPatch) as Network
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Raw.Node (id, inletsCount, outletsCount, shape) as RawNode

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, optimize, toTaggedNdfCode, snocOp, documentationFor, append) as Ndf
import Noodle.Text.NdfFile.Command (Command, op) as Ndf
import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile.Types (NodeInstanceId) as Ndf
import Noodle.Text.WsMessage (Message, fromMessage, toMessage) as WS

import HydraTk.Lang.Program (Program) as Hydra

import Front.Shared.Bounds (Position, Size)
import Front.Shared.Panels (Which(..)) as Panels
import Web.Class.WebRenderer (class WebLocator)
import Web.Class.WebRenderer (firstLocation, locateNext) as Web
import Front.Shared.DocumentationFocus (DocumentationFocus)
import Front.Shared.WsLocation (host, port) as WSLoc
import Front.Shared.HelpText (Context(..), empty) as HelpText
import Front.Shared.HelpText as HT

import Web.Components.ValueEditor (Def) as ValueEditor
import Web.Components.PatchArea.Types (LockingTask(..), NodesGeometry) as PatchArea
import Web.Components.PatchArea (storeGeometry, updatePosition, modifyPosition) as PatchArea
import Web.Components.SidePanel.Console (LogLine(..)) as Console
import Web.Components.SidePanel.WebSocketStatus as WSPanel
import Web.Components.SidePanel.WebSocketStatus (Status(..)) as WS
import Web.Components.AppScreen.UiMode (UiMode(..))
import Web.Components.NodeBox (nodeUiLayout) as NodeBox
import WebSocket.Types (WebSocket, WebSocketMessage) as WS
import WebSocket.Client.Socket (handle, Def, sendMessage) as WSocket
import Web.Components.AppScreen.KeyboardLogic as KL




type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { size :: Maybe Size
    , zoom :: Number
    , uiMode :: UiMode
    , helpContext :: HelpText.Context
    , network :: Network tk ps fs sr cr m
    , mbCurrentPatch :: Maybe { index :: PatchIndex, id :: Id.PatchR }
    , patches :: Map Id.PatchR (PatchInfo loc ps)
    , mbStatusBarContent :: Maybe T.Tag
    , mbHydraProgram :: Maybe Hydra.Program -- FIXME : should be created by Hydra toolkit itself
    , mbCurrentEditor :: Maybe (Id.NodeR /\ ValueEditor.Def cr)
    , mbCurrentDocumentation :: Maybe (DocumentationFocus sr cr)
    , log :: Array Console.LogLine
    , history :: NdfFile
    , ndfInstances :: Map Ndf.NodeInstanceId Id.NodeR
    , openPanels :: Set Panels.Which
    , wsConnection :: WSocketConnection
    , keyboard :: KL.State
    , families :: Array Id.FamilyR
    }


newtype PatchIndex = PatchIndex Int


type PatchInfo loc ps =
    { index :: PatchIndex
    , lastLocation :: loc
    , nodesGeometry :: PatchArea.NodesGeometry
    , mbState :: Maybe ps
    }


type WSocketConnection =
    { status :: WS.Status
    , mbSocket :: Maybe WS.WebSocket
    , log :: Array WS.Message
    }


init :: forall loc tk ps fs sr cr m. Toolkit.HoldsFamilies sr cr m fs => Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
init toolkit =
    { size : Nothing
    , zoom : 1.0
    , uiMode : TransparentOverlay 0.85
    , helpContext : HelpText.empty
    , network : Network.init toolkit
    , mbCurrentPatch : Nothing
    , patches : Map.empty
    , mbStatusBarContent : Nothing
    , mbHydraProgram : Nothing
    , mbCurrentEditor : Nothing
    , mbCurrentDocumentation : Nothing
    , log : []
    , history : Ndf.init "noodle" 2.0
    , ndfInstances : Map.empty
    , openPanels : Set.fromFoldable [ Panels.Commands, Panels.Documentation, Panels.NextActions, Panels.Tree ]
    , wsConnection :
        { status : WS.Off
        , mbSocket : Nothing
        , log : []
        }
    , keyboard : KL.init
    , families : Toolkit.families toolkit
    }


spawnPatch :: forall tk ps fs sr cr mp m. MonadEffect m => Toolkit.InitPatchState tk ps m => State _ tk ps fs sr cr mp -> m { state :: ps, patch :: Patch ps fs sr cr mp }
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    patchState <- Toolkit.initPatch (Proxy :: _ tk)
    Patch.make ("Patch " <> show nextPatchIndex) patchState
        <#> \newPatch -> { patch : newPatch, state : patchState }


registerPatch :: forall loc tk ps fs sr cr m. WebLocator loc => ps -> Patch ps fs sr cr m -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
registerPatch patchState newPatch s =
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = PatchIndex $ patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
        patchR = Patch.id newPatch
        newPatchInfo = initPatchInfo (Proxy :: _ loc) nextPatchIndex patchState
    in
        s
            { patches = s.patches # Map.insert patchR newPatchInfo
            , network = nextNW
            , mbCurrentPatch = Just { index : nextPatchIndex, id : patchR } -- FIXME: make patch current in a separate function
            }


lastPatchIndex :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> PatchIndex
lastPatchIndex s = PatchIndex $ Network.patchesCount s.network


indexOfPatch :: forall tk ps fs sr cr m. Id.PatchR -> State _ tk ps fs sr cr m -> Maybe PatchIndex
indexOfPatch patchR = patchInfo patchR >>> map _.index


patch :: forall tk ps fs sr cr m. Id.PatchR -> State _ tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


patchInfo :: forall loc tk ps fs sr cr m. Id.PatchR -> State loc tk ps fs sr cr m -> Maybe (PatchInfo loc ps)
patchInfo patchR = _.patches >>> Map.lookup patchR


withPatchInfo :: forall loc tk ps fs sr cr m. Id.PatchR -> (PatchInfo loc ps -> PatchInfo loc ps) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withPatchInfo patchR f s = s { patches = Map.update' f patchR s.patches }


currentPatchInfo :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Maybe (PatchInfo loc ps)
currentPatchInfo s = currentPatchId s >>= flip Map.lookup s.patches


withCurrentPatchInfo :: forall loc tk ps fs sr cr m. (PatchInfo loc ps -> PatchInfo loc ps) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withCurrentPatchInfo f s = currentPatchId s <#> (\patchR -> s { patches = Map.update' f patchR s.patches }) # fromMaybe s


currentPatch :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.mbCurrentPatch <#> _.id >>= flip patch s


currentPatchId :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> Maybe Id.PatchR
currentPatchId s = s.mbCurrentPatch <#> _.id


currentPatchState :: forall tk ps fs sr cr mp m. MonadEffect m => State _ tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


currentPatchState' :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> Maybe ps
currentPatchState' = currentPatchInfo >=> _.mbState


withPatch :: forall tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall tk ps fs sr cr m. Id.PatchR -> Patch ps fs sr cr m -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
withCurrentPatch f s = case s.mbCurrentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


initPatchInfo :: forall loc ps. WebLocator loc => Proxy loc -> PatchIndex -> ps -> PatchInfo loc ps
initPatchInfo _ pIndex pState =
    { index : pIndex
    , lastLocation : Web.firstLocation
    , nodesGeometry : Map.empty
    , mbState : Just pState
    }


storeLocation :: forall loc ps. loc -> PatchInfo loc ps -> PatchInfo loc ps
storeLocation loc = _ { lastLocation = loc }


nextLocation :: forall loc ps. WebLocator loc => Size -> PatchInfo loc ps -> loc /\ Position
nextLocation size = _.lastLocation >>> flip Web.locateNext size


newNodeRect :: Size
newNodeRect =
    { width : 300.0, height : 70.0 }


dirDelta = 25.0 :: Number


applyDirToNodePosition :: KL.Dir -> Position -> Position
applyDirToNodePosition = case _ of
    KL.DUp ->    \r -> r { top = r.top - dirDelta }
    KL.DDown ->  \r -> r { top = r.top + dirDelta }
    KL.DLeft ->  \r -> r { left = r.left - dirDelta }
    KL.DRight -> \r -> r { left = r.left + dirDelta }


registerNewNode :: forall loc tk ps fs sr cr m. WebLocator loc => Id.PatchR -> Raw.Node sr cr m -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
registerNewNode patchR rawNode =
    let
        nodeR = RawNode.id rawNode
    in withPatch patchR (Patch.registerRawNode rawNode)
        >>> withPatchInfo patchR \info ->
            let
                nextLoc /\ nodePos = Web.locateNext info.lastLocation newNodeRect
            in info
                { lastLocation = nextLoc
                , nodesGeometry =
                    PatchArea.storeGeometry
                        nodeR
                        { left : nodePos.left, top : nodePos.top
                        , width : newNodeRect.width, height : newNodeRect.height
                        }
                        (NodeBox.nodeUiLayout $ RawNode.shape rawNode)
                    info.nodesGeometry
                }


modifyNodePosition :: forall tk ps fs sr cr m. Id.PatchR -> Id.NodeR -> (Position -> Position) -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
modifyNodePosition patchR nodeR changeF = withPatchInfo patchR $ \info -> info { nodesGeometry = info.nodesGeometry # PatchArea.modifyPosition nodeR changeF }


updateNodePosition :: forall tk ps fs sr cr m. Id.PatchR -> Id.NodeR -> Position -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
updateNodePosition patchR nodeR pos = withPatchInfo patchR $ \info -> info { nodesGeometry = info.nodesGeometry # PatchArea.updatePosition nodeR pos }


log :: forall tk ps fs sr cr m. String -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
log logLine s = s { log = Array.snoc s.log $ Console.LogLine logLine }


logSome :: forall tk ps fs sr cr m. Array String -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
logSome logLines s = s { log = s.log <> (Console.LogLine <$> logLines) }


-- formatHistory :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> Array T.Tag
-- formatHistory = _.history >>> Ndf.optimize >>> Ndf.toTaggedNdfCode >>> pure

formatHistory :: NdfFile -> Array T.Tag
formatHistory = Ndf.optimize >>> Ndf.toTaggedNdfCode >>> pure


trackCommand :: forall loc tk ps fs sr cr m. Ndf.Command -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
trackCommand = trackCommandOp <<< Ndf.op


trackCommandOp :: forall loc tk ps fs sr cr m. Ndf.CommandOp -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
trackCommandOp cmdop s =
    s { history = Ndf.optimize $ flip Ndf.snocOp cmdop $ s.history }


clearHistory :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
clearHistory = _ { history = Ndf.init "noodle" 2.0 }


appendHistory :: forall loc tk ps fs sr cr m. NdfFile -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
appendHistory ndfFile s = s { history = Ndf.append s.history ndfFile }


prependHistory :: forall loc tk ps fs sr cr m. NdfFile -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
prependHistory ndfFile s = s { history = Ndf.append ndfFile s.history }


rememberNdfInstance :: forall loc tk ps fs sr cr m. Id.NodeR -> Ndf.NodeInstanceId -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
rememberNdfInstance nodeR instanceId s = s { ndfInstances = s.ndfInstances # Map.insert instanceId nodeR }


forgetNdfInstance :: forall loc tk ps fs sr cr m. Ndf.NodeInstanceId -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
forgetNdfInstance instanceId s = s { ndfInstances = s.ndfInstances # Map.delete instanceId}


findNdfInstance :: forall loc tk ps fs sr cr m. Id.NodeR -> State loc tk ps fs sr cr m -> Maybe Ndf.NodeInstanceId
findNdfInstance nodeR = _.ndfInstances >>> Map.lookupKey nodeR


findIdForNdfInstance :: forall loc tk ps fs sr cr m. Ndf.NodeInstanceId -> State loc tk ps fs sr cr m -> Maybe Id.NodeR
findIdForNdfInstance instanceId = _.ndfInstances >>> Map.lookup instanceId


switchDocumentation :: forall tk ps fs sr cr m. Id.NodeR -> Maybe (Raw.NodeChanges sr cr) -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
switchDocumentation nodeR mbUpdate s = s { mbCurrentDocumentation = Just { node : nodeR, curUpdate : mbUpdate } }


clearDocumentation :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
clearDocumentation = _ { mbCurrentDocumentation = Nothing }


markWSWaiting :: forall tk ps fs sr cr m. WS.WebSocket -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
markWSWaiting socket s = s
    { wsConnection =
        { log : s.wsConnection.log
        , mbSocket : Just socket
        , status : WS.Waiting
        }
    }


markWSConnected :: forall tk ps fs sr cr m. WS.WebSocket -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
markWSConnected socket s = s
    { wsConnection =
        { log : s.wsConnection.log
        , mbSocket : Just socket
        , status : WS.Connected Nothing { total : 1 }
        }
    }


markWSError :: forall tk ps fs sr cr m. WS.WebSocket -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
markWSError socket s = s
    { wsConnection =
        { log : s.wsConnection.log
        , mbSocket : Just socket
        , status : WS.Error
        }
    }


markWSDisconnect :: forall tk ps fs sr cr m. WS.WebSocket -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
markWSDisconnect socket s = s
    { wsConnection =
        { log : s.wsConnection.log
        , mbSocket : Nothing
        , status : WS.Error
        }
    }


storeWSMessage :: forall tk ps fs sr cr m. WS.Message -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
storeWSMessage msg s = s
    { wsConnection =
        s.wsConnection
            { log = Array.snoc s.wsConnection.log msg }
    }


storeWSNativeMessage :: forall tk ps fs sr cr m. WS.WebSocketMessage -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
storeWSNativeMessage =
    storeWSMessage <<< WS.fromMessage


sendWSMessage :: forall tk ps fs sr cr m. MonadEffect m => WS.Message -> State _ tk ps fs sr cr m -> m Unit
sendWSMessage = sendWSNativeMessage <<< WS.toMessage


sendWSNativeMessage :: forall tk ps fs sr cr m. MonadEffect m => WS.WebSocketMessage -> State _ tk ps fs sr cr m -> m Unit
sendWSNativeMessage msg state =
    case state.wsConnection.mbSocket of
        Just socket ->
            liftEffect $ WSocket.sendMessage socket msg
        Nothing ->
            pure unit


loadWSState :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> WSPanel.State
loadWSState state =
    { log : state.wsConnection.log
    , host : WSLoc.host
    , port : WSLoc.port
    }


loadKbInput :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> KL.Input
loadKbInput state =
    let
        mbCurrentPatch = currentPatch state
        nodesCount = mbCurrentPatch <#> Patch.nodesCount # fromMaybe 0
        linksCount = mbCurrentPatch <#> Patch.linksCount # fromMaybe 0
        mbCurrentNode = do
            currentPatch <- mbCurrentPatch
            selNodeIdx <- KL.selectedNode state.keyboard
            selNode <- Patch.allNodes currentPatch # flip Array.index selNodeIdx -- FIXME: relies only on the fact that `PatchArea` uses `Patch.allNodes` to enumerate them
            pure { inletsCount : RawNode.inletsCount selNode, outletsCount : RawNode.outletsCount selNode }
        familiesCount = Array.length state.families
        patchesCount = Network.patchesCount state.network
        zoomChanged = state.zoom /= 1.0
    in
        { nodesCount
        , familiesCount
        , patchesCount
        , uiMode : state.uiMode
        , mbCurrentNode
        , valueEditorOpened : isJust state.mbCurrentEditor
        , linksCount
        , zoomChanged
        }


findNodeIndexInCurrentPatch :: forall tk ps fs sr cr m. Id.NodeR -> State _ tk ps fs sr cr m -> Maybe Int
findNodeIndexInCurrentPatch nodeR state =
    currentPatch state >>= \patch ->
        Patch.allNodes patch
            <#> RawNode.id
            # Array.findIndex (_ == nodeR)


resetKeyboardFocus :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
resetKeyboardFocus s = s { keyboard = KL.resetFocus s.keyboard }


type PatchStats =
    { nodesCount :: Int
    , linksCount :: Int
    , lockOn :: PatchArea.LockingTask
    }


nextHelpContext :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> PatchStats -> HelpText.Context
nextHelpContext state pStats =
    HelpText.Context $ Set.fromFoldable $
        let
            kbInput = loadKbInput state
            hasNodes = pStats.nodesCount > 0
            hasLinks = pStats.linksCount > 0
            zoomChanged = state.zoom /= 1.0
            commandInputOpen = KL.isCommandInputOpen state.keyboard
        in
            (KL.nextActions kbInput state.keyboard)
            <>
            ( case state.uiMode of
                OnlyCanvas _ ->
                    [ HT.GeneralInterface HT.KB_ShowInterface ]
                CanvasFullyVisible ->
                    [ HT.GeneralInterface HT.KB_ShowInterface ]
                SolidOverlay _ ->
                    [ HT.GeneralInterface HT.KB_HideInterface
                    , HT.GeneralInterface HT.KB_ToggleTransparentBackground
                    ]
                TransparentOverlay _ ->
                    [ HT.GeneralInterface HT.KB_HideInterface
                    , HT.GeneralInterface HT.KB_ToggleSolidBackground
                    ]
            )
            <>
            ( case state.mbCurrentEditor of
                Just _ ->
                    [ HT.PatchArea $ HT.G_OneNode HT.KB_EditInletValue
                    , HT.PatchArea $ HT.G_OneNode HT.KB_FinishEditingInletValue
                    , HT.PatchArea $ HT.G_OneNode HT.M_FinishEditingInletValue
                    ]
                Nothing ->
                    if hasNodes && not commandInputOpen then
                        [ HT.PatchArea $ HT.G_OneNode HT.M_SpawnValueEditor
                        ]
                    else []
            )
            <>
            [ HT.PatchArea HT.KB_ChangeZoom ]
            <>
            ( if zoomChanged then
                [ HT.PatchArea HT.KB_ResetZoom, HT.PatchArea HT.M_ResetZoom ]
            else
                []
            )
            <>
            ( case pStats.lockOn of
                PatchArea.DraggingNode _ _ ->
                    [ HT.PatchArea $ HT.G_SomeNodes HT.M_FinishDraggingNodes ]
                PatchArea.Connecting _ _ ->
                    [ HT.PatchArea $ HT.M_SelectInletToFinishLink ]
                PatchArea.NoLock ->
                    [] -- TODO
            )



    {-
    if KL.isCommandInputOpen state.keyboard then HelpText.CommandInputOpen
    else case state.mbCurrentEditor of
        Just _ ->
            HelpText.EnteringValue
        Nothing ->
            case state.uiMode of
                OnlyCanvas _ ->
                    HelpText.InterfaceHidden
                _ ->
                    case pStats.lockOn of
                        PatchArea.DraggingNode _ _ ->
                            HelpText.DraggingNode
                        PatchArea.Connecting _ _ ->
                            HelpText.CreatingLink
                        PatchArea.NoLock ->
                            HelpText.Start
                                { hasLinks : pStats.linksCount > 0
                                , hasNodes : pStats.nodesCount > 0
                                , zoomChanged : state.zoom /= 1.0
                                } -}