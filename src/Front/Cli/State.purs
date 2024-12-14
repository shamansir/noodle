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
import Noodle.Patch (make, id, registerRawNode) as Patch
import Noodle.Repr.ChRepr (class FromToChRepr)
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


type State (tk :: ToolkitKey) s (fs :: Families) r m =
    { network :: Network tk s fs r m
    , initPatchesFrom :: s
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


init :: forall tk s fs r m. MonadEffect m => s -> Toolkit tk fs r m -> m (State tk s fs r m)
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


informWsInitialized :: forall tk s fs r m. WSS.WebSocketServer -> State tk s fs r m -> State tk s fs r m
informWsInitialized _ state = state


    -- # Network.addPatch (patchIdFromIndex 0) (Patch.init' CAI.none (Hydra.toolkit :: Hydra.Toolkit Effect))


families :: forall tk s fs r m. Toolkit.HoldsFamilies r m fs => State tk s fs r m -> Array Id.FamilyR
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


registerRawNode :: forall nstate tk s fs repr m. StRepr nstate strepr => Id.PatchR -> Raw.Node nstate repr m -> State tk s fs repr m -> State tk s fs repr m
registerRawNode patchR rawNode s = s
    { network = s.network # Network.withPatch patchR (Patch.registerRawNode rawNode) }


patch :: forall tk s fs r m. Id.PatchR -> State tk s fs r m -> Maybe (Patch s fs r m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall tk s fs r m. State tk s fs r m -> Maybe (Patch s fs r m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


withPatch :: forall tk s fs r m. Id.PatchR -> (Patch s fs r m -> Patch s fs r m) -> State tk s fs r m -> State tk s fs r m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall tk s fs r m. Id.PatchR -> Patch s fs r m -> State tk s fs r m -> State tk s fs r m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall tk s fs r m. (Patch s fs r m -> Patch s fs r m) -> State tk s fs r m -> State tk s fs r m
withCurrentPatch f s = case s.currentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


spawnPatch :: forall tk s fs r mp m. MonadEffect m => State tk s fs r mp -> m (Patch s fs r mp)
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    Patch.make ("Patch " <> show nextPatchIndex) s.initPatchesFrom


registerPatch :: forall tk s fs r m. Patch s fs r m -> State tk s fs r m -> State tk s fs r m
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


lastPatchIndex :: forall tk s fs r m. State tk s fs r m -> Int
lastPatchIndex s = Network.patchesCount s.network


withPanels :: forall tk s fs r m. (SidePanels -> SidePanels) -> State tk s fs r m -> State tk s fs r m
withPanels f s = s { panels = f s.panels }