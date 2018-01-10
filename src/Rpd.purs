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
    | ChangePatch PatchId (PatchMsg n c)


data PatchMsg n c
    = AddNode n NodeId String
    | AddNode' n NodeId
    | RemoveNode NodeId
    | Connect NodeId NodeId InletId OutletId
    | Disconnect NodeId NodeId InletId OutletId
    -- Disable Link
    | ChangeNode NodeId (NodeMsg c)


data NodeMsg c
    = AddInlet c InletId String
    | AddInlet' c InletId
    | AddOutlet c OutletId String
    | AddOutlet' c OutletId
    | RemoveInlet InletId
    | RemoveOutlet OutletId
    -- Hide InletId


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
update (AddPatch id title) network = network |> addPatch id title
update (AddPatch' id) network      = network |> addPatch id id
update (RemovePatch id) network    = network |> removePatch id
update (SelectPatch id) network    = network |> selectPatch id
update DeselectPatch network       = network |> deselectPatch
update (EnterPatch id) network     = network |> enterPatch id
update (ExitPatch id) network      = network |> exitPatch id
update (ChangePatch patchId patchMsg) network@(Network network' _) =
    case network'.patches |> Map.lookup patchId of
        Just patch ->
            let
                updatedPatch = patch |> updatePatch patchMsg
                patches' = network'.patches |> Map.insert patchId updatedPatch
                (Patch patch' patchSignal) = updatedPatch
                extractSignal = (\(Patch _ patchSignal) -> patchSignal)
                newPatchSignals =
                    case S.mergeMany (map extractSignal patches') of
                        Just sumSignal -> sumSignal
                        Nothing -> S.constant Bang
            in
                Network
                    network' { patches = patches' }
                    newPatchSignals
        Nothing -> network -- TODO: throw error


updatePatch :: forall n c a x. PatchMsg n c -> Patch n c a x -> Patch n c a x
updatePatch (AddNode type_ id title) patch = patch |> addNode type_ id title
updatePatch (AddNode' type_ id) patch      = patch |> addNode type_ id id
updatePatch (RemoveNode id) patch          = patch |> removeNode id
updatePatch (Connect srcNodeId dstNodeId inletId outletId) patch =
    patch |> connect srcNodeId dstNodeId inletId outletId
updatePatch (Disconnect srcNodeId dstNodeId inletId outletId) patch =
    patch |> disconnect srcNodeId dstNodeId inletId outletId
updatePatch (ChangeNode nodeId nodeMsg) patch@(Patch patch' _) =
    case patch'.nodes |> Map.lookup nodeId of
        Just patch ->
            let
                updatedNode = patch |> updateNode nodeMsg
                nodes' = patch'.nodes |> Map.insert nodeId updatedNode
                (Node node' nodeSignal) = updatedNode
                extractSignal = (\(Node _ nodeSignal) -> nodeSignal)
                newNodeSignals =
                    case S.mergeMany (map extractSignal nodes') of
                        Just sumSignal -> sumSignal
                        Nothing -> S.constant Bang
            in
                Patch
                    patch' { nodes = nodes' }
                    newNodeSignals
        Nothing -> patch -- TODO: throw error


updateNode :: forall n c a x. NodeMsg n c -> Node n c a x -> Node n c a x
updateNode (AddInlet type_ id title) node  = node |> addInlet type_ id title
updateNode (AddInlet' type_ id) node       = node |> addInlet type_ id id
updateNode (AddOutlet type_ id title) node = node |> addOutlet type_ id title
updateNode (AddOutlet' type_ id) node      = node |> addInlet type_ id id
--updateNode (RemoveInlet id) node           = node |> removeInlet id
--updateNode (RemoveOutlet id) node          = node |> removeOutlet id
-- TODO: Send etc


-- Send, Attach etc.


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
        Just (Patch patch' patchSignal) ->
            let
                nodeSignal = S.constant Bang
                node =
                    Node
                        { id : id
                        , title : title
                        , type : type_
                        , inlets : Map.empty
                        , outlets : Map.empty
                        }
                        nodeSignal
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
        Nothing -> network -- return network unchanged in case of error. FIXME: return maybe


removeNode :: forall n c a x. PatchId -> NodeId -> Network n c a x -> Network n c a x
removeNode patchId nodeId network@(Network network' networkSignal) =
    network -- TODO: implement


addInlet
    :: forall n c a x
     . PatchId
    -> NodeId
    -> c
    -> InletId
    -> String
    -> Network n c a x
    -> Network n c a x
addInlet patchId nodeId type_ id label network@(Network network' networkPool) =
    case network'.patches |> Map.lookup patchId of
        Just (Patch patch' patchSignal) ->
            case patch'.nodes |> Map.lookup nodeId of
                Just (Node node' nodeSignal) ->
                    let
                        inletSignal = S.constant Bang
                        inlet =
                            Inlet
                                { id : id
                                , label : label
                                , type : type_
                                }
                                inletSignal
                        nodeWithNewInlet =
                            Node
                                (node' { inlets = node'.inlets |> insert id inlet })
                                (S.merge nodeSignal inletSignal)
                        extractSignal = (\(Node _ nodeSignal) -> nodeSignal)
                        nodes' = patch'.nodes |> delete nodeId
                        newNodeSignals =
                            -- FIXME: rewrite with map?
                            case S.mergeMany (map extractSignal nodes') of
                                Just sumSignal -> sumSignal
                                Nothing -> S.constant Bang
                        patchWithNoNode =
                            Patch
                                (patch' { nodes = nodes' })
                                newNodeSignals
                        patchWithNewNode =
                            case patchWithNoNode of
                                Patch patch' patchSignal ->
                                    Patch
                                        patch' { nodes =
                                            patch'.nodes |> insert nodeId nodeWithNewInlet }
                                        patchSignal
                        networkWithNoPatch =
                            network |> removePatch patchId
                        networkWithNewPatch =
                            network |> addPatch' patchWithNewNode
                    in
                        networkWithNewPatch
                Nothing -> network -- return network unchanged in case of error. FIXME: return maybe
        Nothing -> network -- return network unchanged in case of error. FIXME: return maybe

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
