module Cli.State where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))
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
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (make, id, registerRawNode, registerRawNode') as Patch
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ChRepr (class FromToChRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Node (Node, NodeChanges) as Raw
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, optimize, toTaggedNdfCode, snocOp, documentationFor, append) as Ndf
import Noodle.Text.NdfFile.Command (Command, op) as Ndf
import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.NodeKey (RawNodeKey)

-- import Cli.WsServer as WSS
import Cli.Panels (SidePanelsOnOff, initPanelsOnOff)
import Cli.Panels (Which(..), toggle, isOn) as Panels

import Cli.Bounds (Bounds)
import Cli.Keys as K
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox, removeButton, patchBox) as Key
import Cli.Components.Link (LinkState, LinksFrom, LinksTo)


-- tkey patch-state families node-state-repr channel-value-repr m
type State (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , initPatchesFrom :: ps
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    , wsServer :: Maybe { server :: WSS.WebSocketServer, connection :: Array WSS.WebSocketConnection }
    , lastUpdate :: Map Id.NodeR (Raw.NodeChanges sr cr)
    , lastShift :: { left :: Int, top :: Int }
    , lastClickedOutlet :: Maybe OutletInfo
    , lastLink :: Maybe (LinkState Unit)
    , linksFrom :: LinksFrom Unit
    , linksTo :: LinksTo Unit
    , lastKeys :: LastKeys
    , patchIdToIndex :: Map Id.PatchR Int
    , nodeKeysMap :: Map Id.NodeR K.NodeBoxKey
    , patchKeysMap :: Map Id.PatchR K.PatchBoxKey
    , history :: NdfFile
    , developmentLog :: Array String
    , currentDocumentation :: Array String
    -- TODO, program :: Map Id.NodeIdR Lang.Command
    -- TODO, innerStates :: Map Id.NodeIdR (Ref HoldsNodeState)
    , panelsOnOff :: SidePanelsOnOff
    -- TODO, , editors :: Editors
    -- TODO, , knownGlslFunctions :: Array T.GlslFn
    , blockInletEditor :: Boolean -- temporary hack to handle occasional double clicks on inlets, which could be resolved with event bubbling cancelling support in my PS version of chjj Blessed
    , inletEditorIsOpen :: Boolean
    , locations :: Map Id.NodeR Bounds
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
    }


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


init :: forall tk ps fs sr cr m. MonadEffect m => ps -> Toolkit tk fs sr cr m -> m (State tk ps fs sr cr m)
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
            }
        , lastClickedOutlet : Nothing
        , lastLink : Nothing
        , linksFrom : Map.empty
        , linksTo : Map.empty
        , patchIdToIndex : Map.empty # Map.insert (Patch.id firstPatch) 0
        , nodeKeysMap : Map.empty
        , patchKeysMap : Map.empty -- TODO , patchKeysMap : Map.singleton (patchIdFromIndex 0) Key.patchBox
        , history : Ndf.init "noodle" 2.0
        , developmentLog : []
        , currentDocumentation : []
        , blockInletEditor : false
        , inletEditorIsOpen : false
        -- , program : Map.empty
        -- , innerStates : Map.empty
        -- , nodes : Hydra.noInstances
        , panelsOnOff : initPanelsOnOff
        -- , editors : Map.empty
        -- , knownGlslFunctions : Glsl.knownFns
        -- , linkWasMadeHack : false
        , locations : Map.empty
        }


informWsInitialized :: forall tk ps fs sr cr m. WSS.WebSocketServer -> State tk ps fs sr cr m -> State tk ps fs sr cr m
informWsInitialized _ state = state


    -- # Network.addPatch (patchIdFromIndex 0) (Patch.init' CAI.none (Hydra.toolkit :: Hydra.Toolkit Effect))


families :: forall tk ps fs sr cr m. Toolkit.HoldsFamilies sr cr m fs => State tk ps fs sr cr m -> Array Id.FamilyR
families = _.network >>> Network.toolkit >>> Toolkit.families


nextKeys :: LastKeys -> LastKeys
nextKeys lk =
    { nodeBox      : NodeKey.next lk.nodeBox
    , inletsBox    : NodeKey.next lk.inletsBox
    , outletsBox   : NodeKey.next lk.outletsBox
    , infoBox      : NodeKey.next lk.infoBox
    , removeButton : NodeKey.next lk.removeButton
    }


type NodeBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


registerRawNode :: forall strepr tk ps fs chrepr m. Id.PatchR -> Raw.Node strepr chrepr m -> State tk ps fs strepr chrepr m -> State tk ps fs strepr chrepr m
registerRawNode patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode rawNode) }


registerRawNode' :: forall fstate strepr tk ps fs chrepr m. HasFallback fstate => StRepr fstate strepr => Id.PatchR -> Raw.Node fstate chrepr m -> State tk ps fs strepr chrepr m -> State tk ps fs strepr chrepr m
registerRawNode' patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode' rawNode) }


patch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


currentPatchState :: forall tk ps fs sr cr mp m. MonadEffect m => State tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


withPatch :: forall tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall tk ps fs sr cr m. Id.PatchR -> Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withCurrentPatch f s = case s.currentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


spawnPatch :: forall tk ps fs sr cr mp m. MonadEffect m => State tk ps fs sr cr mp -> m (Patch ps fs sr cr mp)
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    Patch.make ("Patch " <> show nextPatchIndex) s.initPatchesFrom


registerPatch :: forall tk ps fs sr cr m. Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
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


lastPatchIndex :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Int
lastPatchIndex s = Network.patchesCount s.network


storeNodeUpdate :: forall tk ps fs sr cr m. Id.NodeR -> Raw.NodeChanges sr cr -> State tk ps fs sr cr m -> State tk ps fs sr cr m
storeNodeUpdate nodeR changes s = s { lastUpdate = Map.insert nodeR changes s.lastUpdate  }


lastNodeUpdate :: forall tk ps fs sr cr m. Id.NodeR -> State tk ps fs sr cr m -> Maybe (Raw.NodeChanges sr cr)
lastNodeUpdate nodeR = _.lastUpdate >>> Map.lookup nodeR


isPanelOn :: forall tk ps fs sr cr m. Panels.Which -> State tk ps fs sr cr m -> Boolean
isPanelOn which = _.panelsOnOff >>> Panels.isOn which


togglePanel :: forall tk ps fs sr cr m. Panels.Which -> State tk ps fs sr cr m -> State tk ps fs sr cr m
togglePanel which s = s { panelsOnOff = Panels.toggle which s.panelsOnOff }


trackCommand :: forall tk ps fs sr cr m. Ndf.Command -> State tk ps fs sr cr m -> State tk ps fs sr cr m
trackCommand = trackCommandOp <<< Ndf.op


trackCommandOp :: forall tk ps fs sr cr m. Ndf.CommandOp -> State tk ps fs sr cr m -> State tk ps fs sr cr m
trackCommandOp cmdop s =
    s { history = Ndf.optimize $ flip Ndf.snocOp cmdop $ s.history }


formatHistory :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Array T.Tag
formatHistory = _.history >>> Ndf.optimize >>> Ndf.toTaggedNdfCode >>> Array.singleton


clearHistory :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> State tk ps fs sr cr m
clearHistory = _ { history = Ndf.init "noodle" 2.0 }


appendHistory :: forall tk ps fs sr cr m. NdfFile -> State tk ps fs sr cr m -> State tk ps fs sr cr m
appendHistory ndfFile s = s { history = Ndf.append s.history ndfFile }


prependHistory :: forall tk ps fs sr cr m. NdfFile -> State tk ps fs sr cr m -> State tk ps fs sr cr m
prependHistory ndfFile s = s { history = Ndf.append ndfFile s.history }


switchDocumentation :: forall tk ps fs sr cr m. Id.FamilyR -> State tk ps fs sr cr m -> State tk ps fs sr cr m
switchDocumentation familyR s = s { currentDocumentation = s.history # Ndf.documentationFor familyR }


clearDocumentation :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> State tk ps fs sr cr m
clearDocumentation = _ { currentDocumentation = [] }


appendToLog :: forall tk ps fs sr cr m. String -> State tk ps fs sr cr m -> State tk ps fs sr cr m
appendToLog line s = s { developmentLog = Array.snoc s.developmentLog line }


clearLog :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> State tk ps fs sr cr m
clearLog = _ { developmentLog = [] }
