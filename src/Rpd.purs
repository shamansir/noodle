module Rpd
    ( Id, NetworkId, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , Network, Patch, Node, Inlet, Outlet, Link
    , NetworkMsg, update, init
    , addPatch, removePatch, selectPatch, deselectPatch, enterPatch, exitPatch
    , addNode, addInlet, addOutlet, connect, disconnect
    , stringRenderer
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map, insert, delete, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Signal as S
import Data.Function (apply, applyFlipped)

-- Elm-style operators

infixr 0 apply as <|
infixl 1 applyFlipped as |>

type Id = String

type NetworkId = Id
type PatchId = Id
type NodeId = Id
type ChannelId = Id
type InletId = ChannelId
type OutletId = ChannelId
type LinkId = Id

-- `n` — node type
-- `c` — channel type
-- `a` — data type
-- `x` — error type

data NetworkMsg n c
    = AddPatch PatchId String
    | AddPatch' PatchId
    | RemovePatch PatchId
    | SelectPatch PatchId
    | DeselectPatch
    | EnterPatch PatchId
    | ExitPatch PatchId
    | AddNode PatchId n NodeId String
    | AddNode' PatchId n NodeId
    | AddInlet PatchId NodeId c InletId String
    | AddInlet' PatchId NodeId c InletId
    | AddOutlet PatchId NodeId c OutletId String
    | AddOutlet' PatchId NodeId c OutletId
    | Connect PatchId NodeId NodeId InletId OutletId
    | Disconnect PatchId NodeId NodeId InletId OutletId
    -- Hide Inlet'
    -- Disable Link'

data FlowMsg c a x
    = Send (Inlet' c) a -- send data to Outlets?
    | Attach (Inlet' c) (S.Signal a) -- send streams to Outlets?
    | SendError (Inlet' c) x

type Network' n c a x =
    { id :: NetworkId
    , patches :: Map PatchId (Patch n c a x)
    , selected :: Maybe PatchId
    , entered :: Array PatchId
    }

type Patch' n c a x =
    { id :: PatchId
    , title :: String
    , nodes :: Map NodeId (Node n c a x)
    , links :: Map LinkId (Link c a x)
    }

type Node' n c a x =
    { id :: NodeId
    , title :: String
    , type :: n
    , inlets :: Map InletId (Inlet c a x)
    , outlets :: Map OutletId (Outlet c a x)
    }

type Inlet' c =
    { id :: InletId
    , label :: String
    , type :: c
    }

type Outlet' c =
    { id :: OutletId
    , label :: String
    , type :: c
    }

-- type Link' c =
--     { id :: LinkId
--     , inlet :: Inlet' c
--     , outlet :: Outlet' c
--     }

data Flow a x
    = Bang
    | Data a
    | Error x

type FSignal a x = S.Signal (Flow a x)

data Network n c a x = Network (Network' n c a x) (FSignal a x)

data Patch n c a x = Patch (Patch' n c a x) (FSignal a x)

data Node n c a x = Node (Node' n c a x) (FSignal a x)

data Inlet c a x = Inlet (Inlet' c) (FSignal a x)

data Outlet c a x = Outlet (Outlet' c) (FSignal a x)

data Link c a x = Link (Outlet c a x) (Inlet c a x)

-- main functions

init :: forall n c a x. NetworkId -> Network n c a x
init id =
    Network
        { id : id
        , patches : Map.empty
        , selected : Nothing
        , entered : []
        }
        (S.constant Bang)

update :: forall n c a x. NetworkMsg n c -> Network n c a x -> Network n c a x
update (AddPatch id title) = addPatch id title
update (AddPatch' id) = addPatch id id
update (RemovePatch id) = removePatch id
update (SelectPatch id) = selectPatch id
update DeselectPatch = deselectPatch
update (EnterPatch id) = enterPatch id
update (ExitPatch id) = exitPatch id
update (AddNode patchId type_ id title) = addNode patchId type_ id title
update (AddNode' patchId type_ id) = addNode patchId type_ id id
update (AddInlet patchId nodeId type_ id title) = addInlet patchId nodeId type_ id title
update (AddInlet' patchId nodeId type_ id) = addInlet patchId nodeId type_ id id
update (AddOutlet patchId nodeId type_ id title) = addOutlet patchId nodeId type_ id title
update (AddOutlet' patchId nodeId type_ id) = addOutlet patchId nodeId type_ id id
update (Connect patchId srcNodeId dstNodeId inletId outletId) =
    connect patchId srcNodeId dstNodeId inletId outletId
update (Disconnect patchId srcNodeId dstNodeId inletId outletId) =
    disconnect patchId srcNodeId dstNodeId inletId outletId

-- helpers

addPatch :: forall n c a x. PatchId -> String -> Network n c a x -> Network n c a x
addPatch id title network@(Network network' networkSignal) =
    let
        patchSignal = S.constant Bang
        patch =
            Patch
                { id : id
                , title : title
                , nodes : Map.empty
                , links : Map.empty
                }
                patchSignal
    in
        network |> addPatch' patch

addPatch' :: forall n c a x. Patch n c a x -> Network n c a x -> Network n c a x
addPatch' patch@(Patch patch' patchSignal) (Network network' networkSignal) =
    Network
        network' { patches = network'.patches |> insert patch'.id patch }
        (S.merge networkSignal patchSignal)

removePatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
removePatch patchId (Network network' networkSignal) =
    let
        patches' = network'.patches |> delete patchId
        extractSignal = (\(Patch _ patchSignal) -> patchSignal)
        newPatchSignals =
            -- FIXME: rewrite with map?
            case S.mergeMany (map extractSignal patches') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant Bang
    in
        Network
            network' { patches = patches' }
            newPatchSignals

selectPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
selectPatch id (Network network' networkSignal) =
    Network
        network' { selected = Just id }
        networkSignal

deselectPatch :: forall n c a x. Network n c a x -> Network n c a x
deselectPatch (Network network' networkSignal) =
    Network
        network' { selected = Nothing }
        networkSignal

enterPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
enterPatch id (Network network' networkSignal) =
    Network
        network' { entered = id : network'.entered }
        networkSignal

exitPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
exitPatch id (Network network' networkSignal) =
    Network
        network' { entered = Array.delete id network'.entered }
        networkSignal

addNode :: forall n c a x. PatchId -> n -> NodeId -> String -> Network n c a x -> Network n c a x
addNode patchId type_ id title network@(Network network' networkSignal) =
    case network'.patches |> Map.lookup patchId of
        Just patch ->
            let
                nodeSignal = (S.constant Bang)
                node =
                    Node
                        { id : id
                        , title : title
                        , type : type_
                        , inlets : Map.empty
                        , outlets : Map.empty
                        }
                        nodeSignal
                (Patch patch' patchSignal) = patch
                patchWithNewNode =
                    Patch
                       (patch' { nodes = patch'.nodes |> insert id node })
                       (S.merge patchSignal nodeSignal)
                networkWithNoPatch =
                    network |> removePatch patchId
                networkWithNewPatch =
                    network |> addPatch' patchWithNewNode
            in
                networkWithNewPatch
        Nothing -> network -- return network unchanged in case of error. FIXME: return an error

addInlet
    :: forall n c a x
     . PatchId
    -> NodeId
    -> c
    -> InletId
    -> String
    -> Network n c a x
    -> Network n c a x
addInlet patchId nodeId type_ id title (Network network networkPool) =
    (Network network networkPool) -- FIXME: implement

addOutlet
    :: forall n c a x
     . PatchId
    -> NodeId
    -> c
    -> OutletId
    -> String
    -> Network n c a x
    -> Network n c a x
addOutlet patchId nodeId type_ id title (Network network networkPool) =
    (Network network networkPool) -- FIXME: implement

connect
    :: forall n c a x
     . PatchId
    -> NodeId
    -> NodeId
    -> InletId
    -> OutletId
    -> Network n c a x
    -> Network n c a x
connect patchId scrNodeId dstNodeId inletId outletId (Network network networkPool) =
    (Network network networkPool) -- FIXME: implement

disconnect
    :: forall n c a x
     . PatchId
    -> NodeId
    -> NodeId
    -> InletId
    -> OutletId
    -> Network n c a x
    -> Network n c a x
disconnect patchId scrNodeId dstNodeId inletId outletId (Network network networkPool) =
    (Network network networkPool) -- FIXME: implement

stringRenderer :: forall n c a x. Show a => Show x => Patch n c a x -> S.Signal String
stringRenderer (Patch _ patchSignal) =
    patchSignal S.~> (\item ->
        case item of
            Bang -> show "Bang"
            Data d -> show d
             -- make data items require a Show instance,
             -- maybe even everywhere. Also create some type class which defines interfaces
             -- for Node type and Channel type?
            Error x -> show ("Error: " <> (show x)))
