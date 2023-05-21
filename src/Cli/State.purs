module Cli.State where

import Prelude


import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey (RawNodeKey)

import Noodle.Id as Id
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch
import Noodle.Network2 (init, addPatch) as Network

import Cli.Keys (InletsBarKey, NodeBoxKey, OutletsBarKey)
import Cli.Keys (LineA, LineB, LineC) as Key
import Cli.Keys (nodeBox, inletsBar, outletsBar) as Key
import Cli.State.NwWraper (Network, wrapN)

import Toolkit.Hydra2 (toolkit) as Hydra
import Toolkit.Hydra2.Repr.Text (TextRepr) as Hydra


type State =
    { lastInletsBarKey :: InletsBarKey
    , lastNodeBoxKey :: NodeBoxKey
    , lastOutletsBarKey :: OutletsBarKey
    , lastShiftX :: Int
    , lastShiftY :: Int
    , lastClickedOutlet :: Maybe
        { nodeKey :: NodeBoxKey
        , index :: Int
        , subj :: String
        , nodeId :: Id.HoldsNodeId
        , outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.TextRepr
        , node :: Patch.HoldsNode Effect -- Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect
        }
    , lastLink :: Maybe Link
    , linksFrom :: Map RawNodeKey (Map Int Link)
    , linksTo :: Map RawNodeKey (Map Int Link)
    -- , network :: Noodle.Network Unit (Hydra.Families Effect) (Hydra.Instances Effect)
    -- , network :: TestM Effect
    -- , network :: Network (BlessedOpM State Effect)
    , network :: Network Effect
    , currentPatch :: Maybe (Int /\ Patch.Id)
    }


initial :: State
initial =
    { lastShiftX : 0
    , lastShiftY : 0
    , lastNodeBoxKey : Key.nodeBox
    , lastInletsBarKey : Key.inletsBar
    , lastOutletsBarKey : Key.outletsBar
    , lastClickedOutlet : Nothing
    , lastLink : Nothing
    , linksFrom : Map.empty
    , linksTo : Map.empty
    -- , nodes : Hydra.noInstances
    , network : initialNetwork
    , currentPatch : Just (0 /\ patchIdFromIndex 0)
    }


initialNetwork :: forall m. Network m
initialNetwork =
    Network.init Hydra.toolkit
    # Network.addPatch (patchIdFromIndex 0) (Patch.init Hydra.toolkit)
    # wrapN


newtype Link =
    Link
    { id :: Int
    , blessed :: { a :: Core.Blessed State, b :: Core.Blessed State, c :: Core.Blessed State }
    , fromNode :: NodeBoxKey
    , toNode :: NodeBoxKey
    , outletIndex :: Int
    , inletIndex :: Int
    , keys ::
        { a :: Key.LineA
        , b :: Key.LineB
        , c :: Key.LineC
        }
    }

derive instance Newtype Link _


type LinkLineParams =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


type NodePositions =
    { fromNodeLeft :: Int
    , fromNodeTop :: Int
    , toNodeLeft :: Int
    , toNodeTop :: Int
    }


type LinkCalc =
    { a :: LinkLineParams
    , b :: LinkLineParams
    , c :: LinkLineParams
    }


newtype OutletIndex = OutletIndex Int
newtype InletIndex = InletIndex Int


patchIdFromIndex :: Int -> String
patchIdFromIndex = (+) 1 >>> show >>> (<>) "Patch "