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
import Noodle.Network (init, addPatch, toolkit) as Network
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (make, id) as Patch

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.NodeKey (RawNodeKey)

import Cli.WsServer as WSS

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
    , onOff ::
        { commandBox :: Boolean
        , hydraCode :: Boolean
        , fullInfo :: Boolean
        }
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
        , onOff :
            { commandBox : false
            , hydraCode : false
            , fullInfo : false
            }
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