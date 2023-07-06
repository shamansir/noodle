module Cli.State where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)
import Data.Array ((:))

import Control.Monad.State.Class (class MonadState, modify_)

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey (RawNodeKey)

import Noodle.Id as Id
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch
import Noodle.Network2 (init, addPatch) as Network

import Cli.Keys (InletsBoxKey, NodeBoxKey, OutletsBoxKey, InfoBoxKey)
import Cli.Keys (LineA, LineB, LineC) as Key
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox) as Key
import Cli.State.NwWraper (Network, wrapN)
import Cli.Components.NodeBox.HoldsNodeState (HoldsNodeState)

import Noodle.Text.NetworkFile.Command (Command)

import Toolkit.Hydra2 (toolkit, Toolkit) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Lang (Program)
import Toolkit.Hydra2.Lang (empty) as Program


type State =
    { network :: Network Effect
    , currentPatch :: Maybe (Int /\ Patch.Id)
    , lastShiftX :: Int
    , lastShiftY :: Int
    , lastClickedOutlet :: Maybe OutletInfo
    , lastLink :: Maybe Link
    , linksFrom :: Map RawNodeKey (Map Int Link)
    , linksTo :: Map RawNodeKey (Map Int Link)
    , lastKeys :: LastKeys
    , nodeKeysMap :: Map Id.NodeIdR NodeBoxKey
    , commandLog :: Array Command
    , program :: Program Unit
    , innerStates :: Map Id.NodeIdR (Ref HoldsNodeState)
    -- , network :: Noodle.Network Unit (Hydra.Families Effect) (Hydra.Instances Effect)
    -- , network :: TestM Effect
    -- , network :: Network (BlessedOpM State Effect)
    }


initial :: State
initial =
    { network : initialNetwork
    , currentPatch : Just (0 /\ patchIdFromIndex 0)
    , lastShiftX : 0
    , lastShiftY : 0
    , lastKeys :
        { nodeBox : Key.nodeBox
        , inletsBox : Key.inletsBox
        , outletsBox : Key.outletsBox
        , infoBox : Key.infoBox
        }
    , lastClickedOutlet : Nothing
    , lastLink : Nothing
    , linksFrom : Map.empty
    , linksTo : Map.empty
    , nodeKeysMap : Map.empty
    , commandLog : []
    , program : Program.empty
    , innerStates : Map.empty
    -- , nodes : Hydra.noInstances
    }


initialNetwork :: Network Effect
initialNetwork =
    Network.init Hydra.toolkit
    # Network.addPatch (patchIdFromIndex 0) (Patch.init (Hydra.toolkit :: Hydra.Toolkit Effect))
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


type OutletInfo =
    { nodeKey :: NodeBoxKey
    , index :: Int
    , subj :: String
    , nodeId :: Id.HoldsNodeId
    , outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr
    , node :: Patch.HoldsNode Effect -- Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect
    }


type LastKeys =
    { inletsBox :: InletsBoxKey
    , nodeBox :: NodeBoxKey
    , outletsBox :: OutletsBoxKey
    , infoBox :: InfoBoxKey
    }


logCommand :: Command -> State -> State
logCommand cmd state = state { commandLog = cmd : state.commandLog }


logCommandM :: forall m. MonadState State m => Command -> m Unit
logCommandM = modify_ <<< logCommand


logCommandByRef :: forall m. MonadEffect m => Command -> Ref State -> m Unit
logCommandByRef cmd = liftEffect <<< Ref.modify_ (logCommand cmd)