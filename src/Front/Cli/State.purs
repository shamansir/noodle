module Cli.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)

import Web.Socket.Server as WSS

import Noodle.Id as Id
import Noodle.Network (Network)
import Noodle.Toolkit.Families (Families)

import Cli.Keys (NodeBoxKey, PatchBoxKey)


type State pstate (families :: Families) repr m =
    { network :: Network pstate families repr m
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    , wsServer :: Maybe { server :: WSS.WebSocketServer, connection :: Array WSS.WebSocketConnection }
    , lastShift :: { x :: Int, y :: Int }
    -- TODO, lastClickedOutput :: Maybe OutputInfo
    -- TODO, lastLink :: Maybe LinkState
    -- TODO, linksFrom :: Map RawNodeKey (Map OutputIndex LinkState)
    -- TODO, linksTo :: Map RawNodeKey (Map InputIndex LinkState)
    -- TODO, lastKeys :: LastKeys
    , nodeKeysMap :: Map Id.NodeR NodeBoxKey
    , patchKeysMap :: Map Id.PatchR PatchBoxKey
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

{-
type LastKeys =
    { inputsBox :: InputsBoxKey
    , nodeBox :: NodeBoxKey
    , outputsBox :: OutputsBoxKey
    , infoBox :: InfoBoxKey
    , removeButton :: RemoveButtonKey
    }
-}

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


type NodeBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }