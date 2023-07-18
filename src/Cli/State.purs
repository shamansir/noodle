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

import Cli.Keys (InletsBoxKey, NodeBoxKey, OutletsBoxKey, InfoBoxKey, RemoveButtonKey, PatchBoxKey)
import Cli.Keys (LineA, LineB, LineC) as Key
import Cli.Keys (nodeBox, inletsBox, outletsBox, infoBox, removeButton, patchBox) as Key
import Cli.State.NwWraper (Network, wrapN)
import Cli.Components.NodeBox.HoldsNodeState (HoldsNodeState)

import Noodle.Text.NdfFile.Command (Command) as NdfFile
import Noodle.Text.NdfFile (init, append) as NdfFile
import Noodle.Text.NdfFile (NdfFile)

import Toolkit.Hydra2 (toolkit, Toolkit) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Lang (empty) as Program
import Toolkit.Hydra2.Lang (Program, Command) as Lang


type State =
    { network :: Network Effect
    , currentPatch :: Maybe (Int /\ Patch.Id)
    , lastShiftX :: Int
    , lastShiftY :: Int
    , lastClickedOutlet :: Maybe OutletInfo
    , lastLink :: Maybe LinkState
    , linksFrom :: Map RawNodeKey (Map Int LinkState)
    , linksTo :: Map RawNodeKey (Map Int LinkState)
    , lastKeys :: LastKeys
    , nodeKeysMap :: Map Id.NodeIdR NodeBoxKey
    , patchKeysMap :: Map Patch.Id PatchBoxKey
    , commandLog :: NdfFile
    , program :: Map Id.NodeIdR Lang.Command
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
        , removeButton : Key.removeButton
        }
    , lastClickedOutlet : Nothing
    , lastLink : Nothing
    , linksFrom : Map.empty
    , linksTo : Map.empty
    , nodeKeysMap : Map.empty
    , patchKeysMap : Map.singleton (patchIdFromIndex 0) Key.patchBox
    , commandLog : NdfFile.init "hydra" 0.1
    , program : Map.empty
    , innerStates : Map.empty
    -- , nodes : Hydra.noInstances
    }


initialNetwork :: Network Effect
initialNetwork =
    Network.init Hydra.toolkit
    # Network.addPatch (patchIdFromIndex 0) (Patch.init (Hydra.toolkit :: Hydra.Toolkit Effect))
    # wrapN


newtype LinkState =
    LinkState
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

derive instance Newtype LinkState _


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
    , removeButton :: RemoveButtonKey
    }


logLangCommand :: Id.NodeIdR -> Lang.Command -> State -> State
logLangCommand nodeId cmd state = state { program = Map.insert nodeId cmd state.program }


logLangCommandM :: forall m. MonadState State m => Id.NodeIdR -> Lang.Command -> m Unit
logLangCommandM nodeId = modify_ <<< logLangCommand nodeId


logLangCommandByRef :: forall m. MonadEffect m => Id.NodeIdR -> Lang.Command -> Ref State -> m Unit
logLangCommandByRef nodeId cmd = liftEffect <<< Ref.modify_ (logLangCommand nodeId cmd)


logNdfCommand :: NdfFile.Command -> State -> State
logNdfCommand cmd state = state { commandLog = NdfFile.append cmd state.commandLog }


logNdfCommandM :: forall m. MonadState State m => NdfFile.Command -> m Unit
logNdfCommandM = modify_ <<< logNdfCommand


logNdfCommandByRef :: forall m. MonadEffect m => NdfFile.Command -> Ref State -> m Unit
logNdfCommandByRef cmd = liftEffect <<< Ref.modify_ (logNdfCommand cmd)