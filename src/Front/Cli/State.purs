module Cli.State where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..), isJust)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Array (singleton, snoc) as Array
import Data.Text.Format as T

import Type.Proxy (Proxy(..))

import Web.Socket.Server as WSS

import Noodle.Id as Id
import Noodle.Network (Network)
import Noodle.Network (init, patch, addPatch, withPatch, patchesCount, toolkit) as Network
import Noodle.Patch (getState) as Patch
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies, class HoldsFamilies) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (make, id, registerRawNode, registerRawNode') as Patch
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Raw.Fn.Shape (ValueTag) as Shape
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, optimize, toTaggedNdfCode, snocOp, documentationFor, append) as Ndf
import Noodle.Text.NdfFile.Command (Command, op) as Ndf
import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile.Types (NodeInstanceId) as Ndf


import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.NodeKey (RawNodeKey)

-- import Cli.WsServer as WSS
import Cli.Panels (SidePanelsOnOff, initPanelsOnOff)
import Cli.Panels (Which(..), toggle, isOn) as Panels

import Cli.Bounds (Bounds)
import Cli.Keys as K
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox, removeButton, bodyOverlay, patchBox) as Key
import Cli.Components.Link (LinkCmpState)
import Cli.Class.CliRenderer (class CliLocator, firstLocation)


data Focus
    = Patch Id.PatchR
    | Node Id.PatchR Id.NodeR
    | Inlet Id.PatchR Id.NodeR Id.InletR
    | Outlet Id.PatchR Id.NodeR Id.OutletR
    -- InletEditor
    -- RemoveButton
    -- ...

{- TODO,
data Focus
    = Patch
    | Library (Maybe Id.FamilyR)
    | Node Id.NodeR
    | Inlet Id.NodeR Id.InletR
    | Outlet Id.NodeR Id.InletR
    | Link Id.Link
    | SidePanel Which
-}


-- tkey patch-state families node-state-repr channel-value-repr m
type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , initPatchesFrom :: ps
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    , wsServer :: Maybe { server :: WSS.WebSocketServer, connection :: Array WSS.WebSocketConnection }
    , lastUpdate :: Map Id.NodeR (Raw.NodeChanges sr cr)
    , lastShift :: { left :: Int, top :: Int }
    , lastClickedOutlet :: Maybe OutletInfo
    , lastLink :: Maybe (LinkCmpState Unit) -- it is needed for the next Blessed keys for link
    , links :: Map Id.LinkR (LinkCmpState Unit) -- links should have unique IDs across patches so we don't risk anything, arent't we?
    , lastKeys :: LastKeys
    , patchIdToIndex :: Map Id.PatchR Int
    , nodeKeysMap :: Map Id.NodeR K.NodeBoxKey
    , patchKeysMap :: Map Id.PatchR K.PatchBoxKey
    , history :: NdfFile
    , developmentLog :: Array String
    , currentDocumentation :: Maybe (DocumentationFocus sr cr)
    -- TODO, program :: Map Id.NodeIdR Lang.Command
    -- TODO, innerStates :: Map Id.NodeIdR (Ref HoldsNodeState)
    , panelsOnOff :: SidePanelsOnOff
    , commandBoxActive :: Boolean
    -- TODO, , editors :: Editors
    -- TODO, , knownGlslFunctions :: Array T.GlslFn
    , blockInletEditor :: Boolean -- temporary hack to handle occasional double clicks on inlets, which could be resolved with event bubbling cancelling support in my PS version of chjj Blessed
    , inletEditorCreated :: Map Shape.ValueTag Unit
    , inletEditorOpenedFrom :: Maybe (Raw.Node sr cr m /\ Id.InletR) -- TODO: find a way not to store the node instance here
    , lastLocation :: loc
    , locations :: Map Id.NodeR Bounds
    , mouseOverFocus :: Maybe Focus
    , ndfInstances :: Map Ndf.NodeInstanceId Id.NodeR
    }


type OutletInfo =
    { nodeKey :: K.NodeBoxKey
    , index :: Int
    -- , subj :: String
    , nodeId :: Id.NodeR
    , outletId :: Id.OutletR
    -- , node :: Patch.HoldsNode Effect
    }


type LastKeys =
    { inletsBox :: K.InletsBoxKey
    , nodeBox :: K.NodeBoxKey
    , outletsBox :: K.OutletsBoxKey
    , infoBox :: K.InfoBoxKey
    , removeButton :: K.RemoveButtonKey
    , bodyOverlay :: K.BodyOverlayKey
    }


type DocumentationFocus sr cr =
    { node :: Id.NodeR
    , curUpdate :: Maybe (Raw.NodeChanges sr cr)
    }


init :: forall loc tk ps fs sr cr m. MonadEffect m => CliLocator loc => ps -> Toolkit tk fs sr cr m -> m (State loc tk ps fs sr cr m)
init state toolkit = do
    firstPatch <- Patch.make "Patch 1" state
    pure
        { network : Network.init toolkit # Network.addPatch firstPatch
        , currentPatch : Just { index : 0, id : Patch.id firstPatch }
        , initPatchesFrom : state
        , wsServer : Nothing
        , lastUpdate : Map.empty
        , lastShift : { left : 0, top : 0 }
        , lastKeys :
            { nodeBox : Key.nodeBox
            , inletsBox : Key.inletsBox
            , outletsBox : Key.outletsBox
            , infoBox : Key.infoBox
            , removeButton : Key.removeButton
            , bodyOverlay : Key.bodyOverlay
            }
        , lastClickedOutlet : Nothing
        , lastLink : Nothing
        , links : Map.empty
        , patchIdToIndex : Map.empty # Map.insert (Patch.id firstPatch) 0
        , nodeKeysMap : Map.empty
        , patchKeysMap : Map.empty -- TODO , patchKeysMap : Map.singleton (patchIdFromIndex 0) Key.patchBox
        , history : Ndf.init "noodle" 2.0
        , developmentLog : []
        , currentDocumentation : Nothing
        , blockInletEditor : false
        , inletEditorCreated : Map.empty
        , inletEditorOpenedFrom : Nothing
        -- , program : Map.empty
        -- , innerStates : Map.empty
        -- , nodes : Hydra.noInstances
        , panelsOnOff : initPanelsOnOff
        , commandBoxActive : false
        -- , editors : Map.empty
        -- , knownGlslFunctions : Glsl.knownFns
        -- , linkWasMadeHack : false
        , lastLocation : firstLocation
        , locations : Map.empty
        , mouseOverFocus : Nothing
        , ndfInstances : Map.empty
        }


informWsInitialized :: forall loc tk ps fs sr cr m. WSS.WebSocketServer -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
informWsInitialized _ state = state


    -- # Network.addPatch (patchIdFromIndex 0) (Patch.init' CAI.none (Hydra.toolkit :: Hydra.Toolkit Effect))


families :: forall loc tk ps fs sr cr m. Toolkit.HoldsFamilies sr cr m fs => State loc tk ps fs sr cr m -> Array Id.FamilyR
families = _.network >>> Network.toolkit >>> Toolkit.families


nextKeys :: LastKeys -> LastKeys
nextKeys lk =
    { nodeBox      : NodeKey.next lk.nodeBox
    , inletsBox    : NodeKey.next lk.inletsBox
    , outletsBox   : NodeKey.next lk.outletsBox
    , infoBox      : NodeKey.next lk.infoBox
    , removeButton : NodeKey.next lk.removeButton
    , bodyOverlay  : NodeKey.next lk.bodyOverlay
    }


type NodeBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


registerRawNode :: forall strepr loc tk ps fs chrepr m. Id.PatchR -> Raw.Node strepr chrepr m -> State loc tk ps fs strepr chrepr m -> State loc tk ps fs strepr chrepr m
registerRawNode patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode rawNode) }


registerRawNode' :: forall fstate strepr loc tk ps fs chrepr m. HasFallback fstate => StRepr fstate strepr => Id.PatchR -> Raw.Node fstate chrepr m -> State loc tk ps fs strepr chrepr m -> State loc tk ps fs strepr chrepr m
registerRawNode' patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode' rawNode) }


patch :: forall loc tk ps fs sr cr m. Id.PatchR -> State loc tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


currentPatchId :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Maybe Id.PatchR
currentPatchId s = currentPatch s <#> Patch.id


currentPatchState :: forall loc tk ps fs sr cr mp m. MonadEffect m => State loc tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


withPatch :: forall loc tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall loc tk ps fs sr cr m. Id.PatchR -> Patch ps fs sr cr m -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall loc tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withCurrentPatch f s = case s.currentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


spawnPatch :: forall loc tk ps fs sr cr mp m. MonadEffect m => State loc tk ps fs sr cr mp -> m (Patch ps fs sr cr mp)
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    Patch.make ("Patch " <> show nextPatchIndex) s.initPatchesFrom


registerPatch :: forall loc tk ps fs sr cr m. Patch ps fs sr cr m -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
registerPatch newPatch s =
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
    in
        s
            { currentPatch = Just { index : nextPatchIndex, id : Patch.id newPatch }
            , patchIdToIndex = s.patchIdToIndex # Map.insert (Patch.id newPatch) nextPatchIndex
            , network = nextNW
            }


lastPatchIndex :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Int
lastPatchIndex s = Network.patchesCount s.network


storeNodeUpdate :: forall loc tk ps fs sr cr m. Id.NodeR -> Raw.NodeChanges sr cr -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
storeNodeUpdate nodeR changes s = s { lastUpdate = Map.insert nodeR changes s.lastUpdate  }


lastNodeUpdate :: forall loc tk ps fs sr cr m. Id.NodeR -> State loc tk ps fs sr cr m -> Maybe (Raw.NodeChanges sr cr)
lastNodeUpdate nodeR = _.lastUpdate >>> Map.lookup nodeR


isPanelOn :: forall loc tk ps fs sr cr m. Panels.Which -> State loc tk ps fs sr cr m -> Boolean
isPanelOn which = _.panelsOnOff >>> Panels.isOn which


togglePanel :: forall loc tk ps fs sr cr m. Panels.Which -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
togglePanel which s = s { panelsOnOff = Panels.toggle which s.panelsOnOff }


trackCommand :: forall loc tk ps fs sr cr m. Ndf.Command -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
trackCommand = trackCommandOp <<< Ndf.op


trackCommandOp :: forall loc tk ps fs sr cr m. Ndf.CommandOp -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
trackCommandOp cmdop s =
    s { history = Ndf.optimize $ flip Ndf.snocOp cmdop $ s.history }


formatHistory :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Array T.Tag
formatHistory = _.history >>> Ndf.optimize >>> Ndf.toTaggedNdfCode >>> Array.singleton


clearHistory :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
clearHistory = _ { history = Ndf.init "noodle" 2.0 }


appendHistory :: forall loc tk ps fs sr cr m. NdfFile -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
appendHistory ndfFile s = s { history = Ndf.append s.history ndfFile }


prependHistory :: forall loc tk ps fs sr cr m. NdfFile -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
prependHistory ndfFile s = s { history = Ndf.append ndfFile s.history }


switchDocumentation :: forall loc tk ps fs sr cr m. Id.NodeR -> Maybe (Raw.NodeChanges sr cr) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
switchDocumentation nodeR mbUpdate s = s { currentDocumentation = Just { node : nodeR, curUpdate : mbUpdate } }


clearDocumentation :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
clearDocumentation = _ { currentDocumentation = Nothing }


appendToLog :: forall loc tk ps fs sr cr m. String -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
appendToLog line s = s { developmentLog = Array.snoc s.developmentLog line }


clearLog :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
clearLog = _ { developmentLog = [] }


inletEditorCreated :: forall loc tk ps fs sr cr m. Shape.ValueTag -> State loc tk ps fs sr cr m -> Boolean
inletEditorCreated tag = _.inletEditorCreated >>> Map.lookup tag >>> isJust


markInletEditorCreated :: forall loc tk ps fs sr cr m. Shape.ValueTag -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
markInletEditorCreated tag s = s { inletEditorCreated = s.inletEditorCreated # Map.insert tag unit }


registerNdfInstance :: forall loc tk ps fs sr cr m. Ndf.NodeInstanceId -> Id.NodeR -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
registerNdfInstance instanceId nodeR s = s { ndfInstances = s.ndfInstances # Map.insert instanceId nodeR }


findNodeIdByNdfInstance :: forall loc tk ps fs sr cr m. Ndf.NodeInstanceId -> State loc tk ps fs sr cr m -> Maybe Id.NodeR
findNodeIdByNdfInstance instanceId = _.ndfInstances >>> Map.lookup instanceId


findNodeKey :: forall loc tk ps fs sr cr m. Id.NodeR -> State loc tk ps fs sr cr m -> Maybe K.NodeBoxKey
findNodeKey nodeR = _.nodeKeysMap >>> Map.lookup nodeR


findLinkState :: forall loc tk ps fs sr cr m. Id.LinkR -> State loc tk ps fs sr cr m -> Maybe (LinkCmpState Unit)
findLinkState linkR = _.links >>> Map.lookup linkR