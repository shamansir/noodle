module Web.Components.AppScreen.State where

import Prelude

import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty, fromFoldable) as Set
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Map.Extra (update') as Map
import Data.Traversable (traverse)
import Data.Text.Format (Tag) as T
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (length, snoc) as Array
import Data.UniqueHash (UniqueHash)

import Noodle.Id (PatchR, NodeR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class InitPatchState, initPatch) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make, getState, registerRawNode) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, patch, addPatch, withPatch) as Network
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Raw.Node (id) as RawNode

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, optimize, toTaggedNdfCode, snocOp, documentationFor, append) as Ndf
import Noodle.Text.NdfFile.Command (Command, op) as Ndf
import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile.Types (NodeInstanceId) as Ndf
import Noodle.Text.WsMessage (Message, fromMessage) as WS

import HydraTk.Lang.Program (Program) as Hydra

import Front.Shared.Bounds (Position, Size)
import Front.Shared.Panels (Which(..)) as Panels
import Web.Class.WebRenderer (class WebLocator)
import Web.Class.WebRenderer (firstLocation, locateNext) as Web
import Front.Shared.DocumentationFocus (DocumentationFocus)
import Front.Shared.WsLocation (host, port) as WSLoc

import Web.Components.ValueEditor (Def) as ValueEditor
import Web.Components.HelpText (Context(..)) as HelpText
import Web.Components.PatchArea (LockingTask(..), NodesBounds, storeBounds, updatePosition) as PatchArea
import Web.Components.SidePanel.Console (LogLine(..)) as Console
import Web.Components.SidePanel.WebSocketStatus as WSPanel
import Web.Components.SidePanel.WebSocketStatus (Status(..)) as WS

import WebSocket.Types (WebSocket, WebSocketMessage) as WS
import WebSocket.Client.Socket (handle, Def) as WSocket


data UiMode
    = CanvasFullyVisible
    | TransparentOverlay Number -- semi-transparent overlay over canvas
    | SolidOverlay UiMode -- solid color over the canvas (canvas not visible), keeps previous mode to get back to it
    | OnlyCanvas UiMode -- UI is hidden, keeps previous mode to get back to it


type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { size :: Maybe Size
    , zoom :: Number
    , uiMode :: UiMode
    , helpText :: Boolean
    , helpContext :: HelpText.Context
    , shiftPressed :: Boolean
    , network :: Network tk ps fs sr cr m
    , mbCurrentPatch :: Maybe { index :: PatchIndex, id :: Id.PatchR }
    , patches :: Map Id.PatchR (PatchInfo loc ps)
    , mbStatusBarContent :: Maybe T.Tag
    , mbHydraProgram :: Maybe Hydra.Program -- FIXME : should be created by Hydra toolkit itself
    , mbCurrentEditor :: Maybe (Id.NodeR /\ ValueEditor.Def cr)
    , mbCurrentDocumentation :: Maybe (DocumentationFocus sr cr)
    , commandInputActive :: Boolean
    , log :: Array Console.LogLine
    , history :: NdfFile
    , openPanels :: Set Panels.Which
    , wsConnection :: WSocketConnection
    }


newtype PatchIndex = PatchIndex Int


type PatchInfo loc ps =
    { index :: PatchIndex
    , lastLocation :: loc
    , nodesBounds :: PatchArea.NodesBounds
    , mbState :: Maybe ps
    }


type PatchStats =
    { nodesCount :: Int
    , linksCount :: Int
    , lockOn :: PatchArea.LockingTask
    }


type WSocketConnection =
    { status :: WS.Status
    , mbSocket :: Maybe WS.WebSocket
    , log :: Array WS.Message
    }


init :: forall loc tk ps fs sr cr m. Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
init toolkit =
    { size : Nothing
    , zoom : 1.0
    , uiMode : TransparentOverlay 0.1
    , shiftPressed : false
    , helpText : true
    , helpContext : HelpText.Unknown
    , network : Network.init toolkit
    , mbCurrentPatch : Nothing
    , patches : Map.empty
    , mbStatusBarContent : Nothing
    , mbHydraProgram : Nothing
    , mbCurrentEditor : Nothing
    , mbCurrentDocumentation : Nothing
    , commandInputActive : false
    , log : []
    , history : Ndf.init "noodle" 2.0
    , openPanels : Set.fromFoldable [ Panels.Commands, Panels.Documentation, Panels.Tree ]
    , wsConnection :
        { status : WS.Off
        , mbSocket : Nothing
        , log : []
        }
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
    , nodesBounds : Map.empty
    , mbState : Just pState
    }


storeLocation :: forall loc ps. loc -> PatchInfo loc ps -> PatchInfo loc ps
storeLocation loc = _ { lastLocation = loc }


nextLocation :: forall loc ps. WebLocator loc => Size -> PatchInfo loc ps -> loc /\ Position
nextLocation size = _.lastLocation >>> flip Web.locateNext size


newNodeRect :: Size
newNodeRect =
    { width : 300.0, height : 70.0 }


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
                , nodesBounds =
                    PatchArea.storeBounds
                        nodeR
                        { left : nodePos.left, top : nodePos.top
                        , width : newNodeRect.width, height : newNodeRect.height
                        }
                    info.nodesBounds
                }


updateNodePosition :: forall tk ps fs sr cr m. Id.PatchR -> Id.NodeR -> Position -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
updateNodePosition patchR nodeR pos = withPatchInfo patchR $ \info -> info { nodesBounds = info.nodesBounds # PatchArea.updatePosition nodeR pos }


nextHelpContext :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> PatchStats -> HelpText.Context
nextHelpContext state pStats =
    if state.commandInputActive then HelpText.CommandInputOpen
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
                                }


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
            { log = s.wsConnection.log <> pure msg }
    }


storeWSNativeMessage :: forall tk ps fs sr cr m. WS.WebSocketMessage -> State _ tk ps fs sr cr m -> State _ tk ps fs sr cr m
storeWSNativeMessage =
    storeWSMessage <<< WS.fromMessage


loadWSState :: forall tk ps fs sr cr m. State _ tk ps fs sr cr m -> WSPanel.State
loadWSState state =
    { log : state.wsConnection.log
    , host : WSLoc.host
    , port : WSLoc.port
    }