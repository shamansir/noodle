module Cli.State where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Tuple.Nested ((/\), type (/\))

import Type.Proxy (Proxy(..))

import Web.Socket.Server as WSS

import Noodle.Id as Id
import Noodle.Network (Network)
import Noodle.Network (init, addPatch, toolkit) as Network
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class MapFamiliesImpl) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (make, id) as Patch

import Cli.WsServer as WSS

import Cli.Keys as K
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox, removeButton, patchBox) as Key


type State (tk :: ToolkitKey) s (fs :: Families) r m =
    { network :: Network tk s fs r m
    , initPatchesFrom :: s
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    , wsServer :: Maybe { server :: WSS.WebSocketServer, connection :: Array WSS.WebSocketConnection }
    , lastShift :: { x :: Int, y :: Int }
    -- TODO, lastClickedOutput :: Maybe OutputInfo
    -- TODO, lastLink :: Maybe LinkState
    -- TODO, linksFrom :: Map RawNodeKey (Map OutputIndex LinkState)
    -- TODO, linksTo :: Map RawNodeKey (Map InputIndex LinkState)
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
    , locations :: Map Id.NodeR NodeBounds
    }

{-
type OutputInfo =
    { nodeKey :: NodeBoxKey
    , index :: Int
    , subj :: String
    , nodeId :: Id.HoldsNodeId
    , outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr
    , node :: Patch.HoldsNode Effect -- Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect
    }
-}


type LastKeys =
    { inletsBox :: K.InletsBoxKey
    , nodeBox :: K.NodeBoxKey
    , outletsBox :: K.OutletsBoxKey
    , infoBox :: K.InfoBoxKey
    , removeButton :: K.RemoveButtonKey
    }

{-
newtype LinkState =
    LinkState
    { id :: Int
    , inPatch :: Id.LinkId
    , blessed :: { a :: Core.Blessed State, b :: Core.Blessed State, c :: Core.Blessed State }
    , fromNode :: { key :: NodeBoxKey, id :: Id.NodeIdR }
    , toNode :: { key :: NodeBoxKey, id :: Id.NodeIdR }
    , outputIndex :: Int
    , inputIndex :: Int
    , keys ::
        { a :: Key.LineA
        , b :: Key.LineB
        , c :: Key.LineC
        }
    }
-}


init :: forall tk s fs r m. MonadEffect m => s -> Toolkit tk fs r m -> m (State tk s fs r m)
init state toolkit = do
    firstPatch <- Patch.make "Patch 1" state
    pure
        { network : Network.init toolkit # Network.addPatch firstPatch
        , currentPatch : Just { index : 0, id : Patch.id firstPatch }
        , initPatchesFrom : state
        , wsServer : Nothing
        , lastShift : { x : 0, y : 0 }
        , lastKeys :
            { nodeBox : Key.nodeBox
            , inletsBox : Key.inletsBox
            , outletsBox : Key.outletsBox
            , infoBox : Key.infoBox
            , removeButton : Key.removeButton
            }
        -- TODO, , lastClickedOutput : Nothing
        -- TODO, , lastLink : Nothing
        -- TODO, , linksFrom : Map.empty
        -- TODO, , linksTo : Map.empty
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


families :: forall tk s fs r m. Toolkit.MapFamiliesImpl r m fs => State tk s fs r m -> Array Id.FamilyR
families = _.network >>> Network.toolkit >>> Toolkit.families


type NodeBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }