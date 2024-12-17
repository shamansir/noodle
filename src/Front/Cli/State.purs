module Cli.State where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)

import Type.Proxy (Proxy(..))

import Web.Socket.Server as WSS

import Noodle.Id as Id
import Noodle.Network (Network)
import Noodle.Network (init, patch, addPatch, withPatch, patchesCount, toolkit) as Network
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (make, id, registerRawNode, registerRawNode') as Patch
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ChRepr (class FromToChRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Node (Node) as Raw

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.NodeKey (RawNodeKey)

-- import Cli.WsServer as WSS
import Cli.Panels (SidePanels, initPanels)

import Cli.Bounds (Bounds)
import Cli.Keys as K
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox, removeButton, patchBox) as Key
import Cli.Components.Link (LinkState, LinksFrom, LinksTo)


type State (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , initPatchesFrom :: ps
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    , wsServer :: Maybe { server :: WSS.WebSocketServer, connection :: Array WSS.WebSocketConnection }
    , lastShift :: { left :: Int, top :: Int }
    , lastClickedOutlet :: Maybe OutletInfo
    , lastLink :: Maybe (LinkState Unit)
    , linksFrom :: LinksFrom Unit
    , linksTo :: LinksTo Unit
    , lastKeys :: LastKeys
    , patchIdToIndex :: Map Id.PatchR Int
    , nodeKeysMap :: Map Id.NodeR K.NodeBoxKey
    , patchKeysMap :: Map Id.PatchR K.PatchBoxKey
    -- TODO, commandLog :: NdfFile
    -- TODO, program :: Map Id.NodeIdR Lang.Command
    -- TODO, innerStates :: Map Id.NodeIdR (Ref HoldsNodeState)
    , panels :: SidePanels
    -- TODO, , editors :: Editors
    -- TODO, , knownGlslFunctions :: Array T.GlslFn
    -- TODO, , linkWasMadeHack :: Boolean -- hack because inputs / outputs get double click event somehow FIXME: get rid of
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


init :: forall tk ps fs sr cr m. MonadEffect m => ps -> Toolkit tk fs sr cr m -> m (State tk ps fs sr cr m)
init state toolkit = do
    firstPatch <- Patch.make "Patch 1" state
    pure
        { network : Network.init toolkit # Network.addPatch firstPatch
        , currentPatch : Just { index : 0, id : Patch.id firstPatch }
        , initPatchesFrom : state
        , wsServer : Nothing
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
        -- , commandLog : NdfFile.init "hydra" 0.1
        -- , program : Map.empty
        -- , innerStates : Map.empty
        -- , nodes : Hydra.noInstances
        , panels : initPanels
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


registerRawNode :: forall fstate strepr tk ps fs chrepr m. Id.PatchR -> Raw.Node strepr chrepr m -> State tk ps fs strepr chrepr m -> State tk ps fs strepr chrepr m
registerRawNode patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode rawNode) }


registerRawNode' :: forall fstate strepr tk ps fs chrepr m. HasFallback fstate => StRepr fstate strepr => Id.PatchR -> Raw.Node fstate chrepr m -> State tk ps fs strepr chrepr m -> State tk ps fs strepr chrepr m
registerRawNode' patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode' rawNode) }


patch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


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


withPanels :: forall tk ps fs sr cr m. (SidePanels -> SidePanels) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withPanels f s = s { panels = f s.panels }