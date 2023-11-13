module Cli.State where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)
import Data.Array ((:))
import Data.Array as Array
import Data.SProxy (reflect')
import Control.Monad.State.Class (class MonadState, modify_)

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey (RawNodeKey, HoldsNodeKey)
import Blessed.Internal.NodeKey as NK

import Web.Socket.Server (WebSocketServer, WebSocketConnection) as WSS

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch as Patch
import Noodle.Patch (Patch)
import Noodle.Network (init, addPatch, withPatch) as Network
import Noodle.Node.HoldsNodeState (HoldsNodeState)
import Noodle.Stateful (set) as Stateful

import Cli.Keys (InputsBoxKey, NodeBoxKey, OutputsBoxKey, InfoBoxKey, RemoveButtonKey, PatchBoxKey)
import Cli.Keys (LineA, LineB, LineC) as Key
import Cli.Keys (nodeBox, inputsBox, outputsBox, infoBox, removeButton, patchBox) as Key
import Cli.State.NwWraper (Network, wrapN, unwrapN)

import Noodle.Text.NdfFile.Command (Command) as NdfFile
import Noodle.Text.NdfFile (init, append) as NdfFile
import Noodle.Text.NdfFile (NdfFile)

import Tookit.Hydra (toolkit, Toolkit, State, Instances) as Hydra
import Tookit.Hydra.Repr.Wrap (WrapRepr) as Hydra
import Tookit.Hydra.Lang (empty) as Program
import Tookit.Hydra.Lang (Program, Command) as Lang
import Tookit.Hydra.Family.Render.Editor (Editors)
import Tookit.Hydra.Types as T
import Tookit.Hydra.Lang.Glsl as Glsl

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel)

import CompArts.Product as CAI


type State =
    { network :: Network Effect
    , currentPatch :: Maybe (Int /\ Patch.Id)
    , wsServer :: Maybe (WSS.WebSocketServer /\ Array WSS.WebSocketConnection)
    , lastShiftX :: Int
    , lastShiftY :: Int
    , lastClickedOutput :: Maybe OutputInfo
    , lastLink :: Maybe LinkState
    , linksFrom :: Map RawNodeKey (Map OutputIndex LinkState)
    , linksTo :: Map RawNodeKey (Map InputIndex LinkState)
    , lastKeys :: LastKeys
    , nodeKeysMap :: Map Id.NodeIdR NodeBoxKey
    , patchKeysMap :: Map Patch.Id PatchBoxKey
    , commandLog :: NdfFile
    , program :: Map Id.NodeIdR Lang.Command
    , innerStates :: Map Id.NodeIdR (Ref HoldsNodeState)
    , commandBoxOn :: Boolean
    , hydraCodeOn :: Boolean
    , fullInfoOn :: Boolean
    , editors :: Editors
    , knownGlslFunctions :: Array T.GlslFn
    , linkWasMadeHack :: Boolean -- hack because inputs / outputs get double click event somehow FIXME: get rid of
    , locations :: Map Id.NodeIdR NodeBounds
    }


initial :: State
initial =
    { network : initialNetwork
    , currentPatch : Just (0 /\ patchIdFromIndex 0)
    , wsServer : Nothing
    , lastShiftX : 0
    , lastShiftY : 0
    , lastKeys :
        { nodeBox : Key.nodeBox
        , inputsBox : Key.inputsBox
        , outputsBox : Key.outputsBox
        , infoBox : Key.infoBox
        , removeButton : Key.removeButton
        }
    , lastClickedOutput : Nothing
    , lastLink : Nothing
    , linksFrom : Map.empty
    , linksTo : Map.empty
    , nodeKeysMap : Map.empty
    , patchKeysMap : Map.singleton (patchIdFromIndex 0) Key.patchBox
    , commandLog : NdfFile.init "hydra" 0.1
    , program : Map.empty
    , innerStates : Map.empty
    -- , nodes : Hydra.noInstances
    , commandBoxOn : false
    , hydraCodeOn : false
    , fullInfoOn : false
    , editors : Map.empty
    , knownGlslFunctions : Glsl.knownFns
    , linkWasMadeHack : false
    , locations : Map.empty
    }


initialNetwork :: Network Effect
initialNetwork =
    Network.init Hydra.toolkit
    # Network.addPatch (patchIdFromIndex 0) (Patch.init' CAI.none (Hydra.toolkit :: Hydra.Toolkit Effect))
    # wrapN


withCurrentPatch :: (Patch Hydra.State (Hydra.Instances Effect) -> Patch Hydra.State (Hydra.Instances Effect)) -> State -> State
withCurrentPatch f state =
    case state.currentPatch of
        Just (_ /\ id) ->
            state { network = state.network # unwrapN # Network.withPatch id f # wrapN }
        Nothing -> state


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

derive instance Newtype LinkState _


type LinkLineBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


type NodeBounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


type FromToBounds =
    { from :: NodeBounds
    , to :: NodeBounds
    }


type LinkCalc =
    { a :: LinkLineBounds
    , b :: LinkLineBounds
    , c :: LinkLineBounds
    }


newtype OutputIndex = OutputIndex Int
newtype InputIndex = InputIndex Int


derive newtype instance Eq OutputIndex
derive newtype instance Ord OutputIndex
derive newtype instance Eq InputIndex
derive newtype instance Ord InputIndex


patchIdFromIndex :: Int -> String
patchIdFromIndex = (+) 1 >>> show >>> (<>) "Patch "


type OutputInfo =
    { nodeKey :: NodeBoxKey
    , index :: Int
    , subj :: String
    , nodeId :: Id.HoldsNodeId
    , outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr
    , node :: Patch.HoldsNode Effect -- Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect
    }


type LastKeys =
    { inputsBox :: InputsBoxKey
    , nodeBox :: NodeBoxKey
    , outputsBox :: OutputsBoxKey
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


informWsInitialized :: WSS.WebSocketServer -> State -> State
informWsInitialized wss state = state { wsServer = Just $ wss /\ [] }


informWsListening :: State -> State
informWsListening state = state


registerWsClient :: WSS.WebSocketConnection -> State -> State
registerWsClient ws state = state { wsServer = addClient <$> state.wsServer }
    where addClient (srv /\ connections) = srv /\ ws : connections


connectionsCount :: State -> Maybe Int
connectionsCount = _.wsServer >>> map (Tuple.snd >>> Array.length)


toggleCommandBox :: State -> State
toggleCommandBox s =
    s { commandBoxOn = not s.commandBoxOn }


toggleHydraCode :: State -> State
toggleHydraCode s =
    s { hydraCodeOn = not s.hydraCodeOn }


toggleFullInfo :: State -> State
toggleFullInfo s =
    s { fullInfoOn = not s.fullInfoOn }


instance Show LinkState where
    show (LinkState ({ id, inPatch, fromNode, toNode, outputIndex, inputIndex })) =
        "(" <> show id <> "," <> show inPatch <> ") "
            <> show (NK.rawify fromNode.key) <> ":" <> reflect' fromNode.id <> ":" <> show outputIndex
            <> " -> "
            <> show (NK.rawify toNode.key) <> ":" <> reflect' toNode.id <> ":" <> show inputIndex