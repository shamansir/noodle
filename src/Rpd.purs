module Rpd
    ( Id, NetworkId, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , Network, Patch, Node, Inlet, Outlet, Link
    , NetworkMsg, update, init
    , addPatch, removePatch, selectPatch, deselectPatch, enterPatch, exitPatch
    , addNode, addInlet, addOutlet, connect, disconnect
    , log--, logData
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Function (apply, applyFlipped)
import Data.Map (Map, insert, delete, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Signal as S

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

data NetworkMsg n c a x
    = CreateNetwork
    | AddPatch PatchId String
    | AddPatch' PatchId
    | RemovePatch PatchId
    | SelectPatch PatchId
    | DeselectPatch
    | EnterPatch PatchId
    | ExitPatch PatchId
    | ChangePatch PatchId (PatchMsg n c a x)
    | ChangePatch' (Patch n c a x)
    | NetworkGotEmpty -- TODO: remove


data PatchMsg n c a x
    = CreatePatch
    | AddNode n NodeId String
    | AddNode' n NodeId
    | RemoveNode NodeId
    | Connect NodeId NodeId OutletId InletId
    | Disconnect NodeId NodeId OutletId InletId
    -- Disable Link
    | ChangeNode NodeId (NodeMsg c a x)
    | ChangeNode' (Node n c a x)
    | PatchGotEmpty -- TODO: remove


data NodeMsg c a x
    = CreateNode
    | AddInlet c InletId String
    | AddInlet' c InletId
    | AddOutlet c OutletId String
    | AddOutlet' c OutletId
    | RemoveInlet InletId
    | RemoveOutlet OutletId
    | ChangeInlet InletId (InletMsg c a x)
    | ChangeInlet' (Inlet c a x)
    | ChangeOutlet OutletId (OutletMsg c a x)
    | ChangeOutlet' (Outlet c a x)
    | NodeGotEmpty -- TODO: remove


-- TODO: use general ChannelMsg / FlowMsg ... ?

data InletMsg c a x
    = CreateInlet
    | ConnectToOutlet (Outlet' c) (FlowSignal a x)
    | DisconnectFromOutlet (Outlet' c)
    | Hide
    -- | Receive a
    -- | Attach c (S.Signal a)
    -- | ReceiveError x



data OutletMsg c a x
    -- = Send a
    -- | Emit c (S.Signal a)
    -- | SendError x
    = CreateOutlet
    | ConnectToInlet (Inlet' c)
    | DisconnectFromInlet (Inlet' c)


data Value a x
    = Bang
    | Data a
    | Error x
    | SysError String


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
    , links :: Array (Link c a x)
    }

type Node' n c a x =
    { id :: NodeId
    , title :: String
    , type :: n
    , process :: Maybe (Map InletId (Value a x) -> Map OutletId (Value a x))
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


-- The signal where all the data flows: Bangs, data chunks and errors
type FlowSignal a x = S.Signal (Value a x)

type TaggedFlowSignal a x = S.Signal (Tuple InletId (Value a x))

-- The signal where the messages go
type MsgSignal m = S.Signal m

-- The special signal for nodes which tracks the data flow through node inputs and outlets
type ProcessSignal a x = S.Signal (Tuple (Map InletId (Value a x)) (Map OutletId (Value a x)))

data Network n c a x = Network (Network' n c a x) (MsgSignal (NetworkMsg n c a x))

data Patch n c a x = Patch (Patch' n c a x) (MsgSignal (PatchMsg n c a x))

data Node n c a x = Node (Node' n c a x) (MsgSignal (NodeMsg c a x)) (ProcessSignal a x)

data Inlet c a x = Inlet (Inlet' c) (MsgSignal (InletMsg c a x)) (FlowSignal a x)

data Outlet c a x = Outlet (Outlet' c) (MsgSignal (OutletMsg c a x)) (FlowSignal a x)

data Link c a x = Link NodeId NodeId OutletId InletId (FlowSignal a x)

initProcessSignal :: forall a x. ProcessSignal a x
initProcessSignal =
    S.constant (Tuple Map.empty Map.empty)

-- main functions

init :: forall n c a x. NetworkId -> Network n c a x
init id =
    Network
        { id : id
        , patches : Map.empty
        , selected : Nothing
        , entered : []
        }
        (S.constant CreateNetwork)


update :: forall n c a x. NetworkMsg n c a x -> Network n c a x -> Network n c a x
update CreateNetwork network       = network
update (AddPatch id title) network = network |> addPatch id title
update (AddPatch' id) network      = network |> addPatch id id
update (RemovePatch id) network    = network |> removePatch id
update (SelectPatch id) network    = network |> selectPatch id
update DeselectPatch network       = network |> deselectPatch
update (EnterPatch id) network     = network |> enterPatch id
update (ExitPatch id) network      = network |> exitPatch id
update NetworkGotEmpty network     = network
update (ChangePatch patchId patchMsg) network@(Network network' _) =
    case network'.patches |> Map.lookup patchId of
        Just patch ->
            update (ChangePatch' (patch |> updatePatch patchMsg)) network
        Nothing -> network -- TODO: throw error
update (ChangePatch' updatedPatch@(Patch patch' _)) network@(Network network' _) =
    let
        patches' = network'.patches |> Map.insert patch'.id updatedPatch
        newPatchSignals =
            case S.mergeMany (map adaptPatchSignal patches') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NetworkGotEmpty
    in
        Network
            network' { patches = patches' }
            newPatchSignals


updatePatch :: forall n c a x. PatchMsg n c a x -> Patch n c a x -> Patch n c a x
updatePatch CreatePatch patch              = patch
updatePatch (AddNode type_ id title) patch = patch |> addNode type_ id title
updatePatch (AddNode' type_ id) patch      = patch |> addNode type_ id id
updatePatch (RemoveNode id) patch          = patch |> removeNode id
updatePatch (Connect srcNodeId dstNodeId inletId outletId) patch =
    patch |> connect srcNodeId dstNodeId inletId outletId
updatePatch (Disconnect srcNodeId dstNodeId inletId outletId) patch =
    patch |> disconnect srcNodeId dstNodeId inletId outletId
updatePatch PatchGotEmpty patch            = patch
updatePatch (ChangeNode nodeId nodeMsg) patch@(Patch patch' _) =
    case patch'.nodes |> Map.lookup nodeId of
        Just node ->
            updatePatch (ChangeNode' (node |> updateNode nodeMsg)) patch
        Nothing -> patch -- TODO: throw error
updatePatch (ChangeNode' updatedNode@(Node node' _ _)) patch@(Patch patch' _) =
    let
        nodes' = patch'.nodes |> Map.insert node'.id updatedNode
        newNodeSignals =
            case S.mergeMany (map adaptNodeSignal nodes') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant PatchGotEmpty
    in
        Patch
            patch' { nodes = nodes' }
            newNodeSignals


updateNode :: forall n c a x. NodeMsg c a x -> Node n c a x -> Node n c a x
updateNode CreateNode node                 = node
updateNode (AddInlet type_ id title) node  = node |> addInlet type_ id title
updateNode (AddInlet' type_ id) node       = node |> addInlet type_ id id
updateNode (AddOutlet type_ id title) node = node |> addOutlet type_ id title
updateNode (AddOutlet' type_ id) node      = node |> addInlet type_ id id
updateNode (RemoveInlet id) node           = node -- |> removeInlet id
updateNode (RemoveOutlet id) node          = node -- |> removeOutlet id
updateNode NodeGotEmpty node               = node
updateNode (ChangeInlet inletId inletMsg) node@(Node node' _ processSignal) =
    case node'.inlets |> Map.lookup inletId of
        Just inlet ->
            updateNode (ChangeInlet' (inlet |> updateInlet inletMsg)) node
        Nothing -> node -- TODO: throw error
updateNode (ChangeInlet' updatedInlet@(Inlet inlet' _ _)) node@(Node node' _ processSignal) =
    let
        inlets' = node'.inlets |> Map.insert inlet'.id updatedInlet
        newInletSignals =
            case S.mergeMany (map adaptInletSignal inlets') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NodeGotEmpty
        processSignal' =
            case node'.process of
                Just processFn ->
                    case S.mergeMany (map tagFlowSignal inlets') of
                        Just taggedSignal ->
                            S.foldp (\(Tuple inletId inletVal) (Tuple inletVals _) ->
                                let
                                    inletVals' = inletVals |> Map.insert inletId inletVal
                                    outletVals' = processFn inletVals'
                                in
                                    Tuple inletVals' outletVals'
                            ) (Tuple Map.empty Map.empty) taggedSignal
                        Nothing -> initProcessSignal
                Nothing -> initProcessSignal -- processSignal
        outlets' = map (\(Outlet outlet' msgSignal flowSignal) ->
            let
                filteredProcessSignal =
                    S.filter
                        (\(Tuple _ outletVals) -> Map.member outlet'.id outletVals)
                        (Tuple Map.empty Map.empty)
                        processSignal'
                flowSignal' =
                    map
                        (\(Tuple _ outletVals) ->
                            case Map.lookup outlet'.id outletVals of
                                Just value -> value
                                Nothing -> SysError "Failed to find outlet"
                        )
                        filteredProcessSignal
            in
                Outlet outlet' msgSignal flowSignal'
        ) node'.outlets
        -- outlets' = map
        --     (\(Outlet outlet' msgSignal flowSignal) ->
        --         let
        --             filteredProcessSignal =
        --                 S.filter
        --                     (Tuple _ outletVals -> Map.member outlet'.id outletVals)
        --                     processSignal'
        --             flowSignal' =
        --                 map
        --                     (\(Tuple _ outletVals) ->
        --                         case Map.lookup outlet'.id outletVals of
        --                             Just value -> value
        --                             Nothing -> SysError "Failed to find outlet"
        --                     )
        --                 filteredProcessSignal
        --         in
        --             Outlet outlet' msgSignal flowSignal'
        --     ) node'.outlets
        outletSignals =
            case S.mergeMany (map adaptOutletSignal node'.outlets) of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NodeGotEmpty
    in
        Node
            node' { inlets = inlets' }
            (S.merge newInletSignals outletSignals)
            processSignal'
updateNode (ChangeOutlet outletId outletMsg) node@(Node node' _ processSignal) =
    case node'.outlets |> Map.lookup outletId of
        Just outlet ->
            updateNode (ChangeOutlet' (outlet |> updateOutlet outletMsg)) node
        Nothing -> node -- TODO: throw error
updateNode (ChangeOutlet' updatedOutlet@(Outlet outlet' _ _)) node@(Node node' _ processSignal) =
    let
        outlets' = node'.outlets |> Map.insert outlet'.id updatedOutlet
        newOutletSignals =
            case S.mergeMany (map adaptOutletSignal outlets') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NodeGotEmpty
        inletSignals =
            case S.mergeMany (map adaptInletSignal node'.inlets) of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NodeGotEmpty
    in
        Node
            node' { outlets = outlets' }
            (S.merge inletSignals newOutletSignals)
            processSignal


updateInlet :: forall c a x. InletMsg c a x -> Inlet c a x -> Inlet c a x
updateInlet CreateInlet inlet = inlet
updateInlet (ConnectToOutlet _ signal) (Inlet inlet' msgSignal dataSignal) =
    Inlet inlet' msgSignal (S.merge dataSignal signal)
updateInlet (DisconnectFromOutlet _) inlet = inlet -- FIXME: implement
updateInlet Hide inlet = inlet


updateOutlet :: forall c a x. OutletMsg c a x -> Outlet c a x -> Outlet c a x
updateOutlet CreateOutlet outlet = outlet
updateOutlet (ConnectToInlet _) outlet = outlet
updateOutlet (DisconnectFromInlet _) outlet = outlet -- FIXME: implement


-- Send, Attach etc.


-- helpers: Network

addPatch :: forall n c a x. PatchId -> String -> Network n c a x -> Network n c a x
addPatch id title network@(Network network' networkSignal) =
    let
        patchSignal = S.constant CreatePatch
        patch@(Patch patch' _) =
            Patch
                { id : id
                , title : title
                , nodes : Map.empty
                , links : []
                }
                patchSignal
    in
        Network
            network' { patches = network'.patches |> insert patch'.id patch }
            (adaptPatchSignal patch |> S.merge networkSignal)


removePatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
removePatch patchId (Network network' networkSignal) =
    let
        patches' = network'.patches |> delete patchId
        extractSignal = (\(Patch patch' patchSignal) ->
            patchSignal S.~> (\patchMsg -> ChangePatch patchId patchMsg))
        newPatchSignals =
            case S.mergeMany (map extractSignal patches') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant NetworkGotEmpty -- TODO: remove
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


-- helpers: Patch

addNode :: forall n c a x. n -> NodeId -> String -> Patch n c a x -> Patch n c a x
addNode type_ id title patch@(Patch patch' patchSignal) =
    let
        nodeSignal = S.constant CreateNode
        node@(Node node' _ _) =
            Node
                { id : id
                , title : title
                , type : type_
                , process : Nothing
                , inlets : Map.empty
                , outlets : Map.empty
                }
                nodeSignal
                (S.constant (Tuple Map.empty Map.empty))
    in
        Patch
            patch' { nodes = patch'.nodes |> insert node'.id node }
            (adaptNodeSignal node |> S.merge patchSignal)


removeNode :: forall n c a x. NodeId -> Patch n c a x -> Patch n c a x
removeNode nodeId patch@(Patch patch' patchSignal) =
    let
        nodes' = patch'.nodes |> delete nodeId
        extractSignal = (\(Node node' nodeSignal _) ->
            nodeSignal S.~> (\nodeMsg -> ChangeNode nodeId nodeMsg))
        newNodeSignals =
            case S.mergeMany (map extractSignal nodes') of
                Just sumSignal -> sumSignal
                Nothing -> S.constant PatchGotEmpty
    in
        Patch
            patch' { nodes = nodes' }
            newNodeSignals


connect
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
connect srcNodeId dstNodeId outletId inletId patch@(Patch patch' patchSignal) =
    case Map.lookup srcNodeId patch'.nodes,
         Map.lookup dstNodeId patch'.nodes of
        Just srcNode@(Node srcNode' _ _),
        Just dstNode@(Node dstNode' _ _) ->
            case Map.lookup outletId srcNode'.outlets,
                 Map.lookup inletId dstNode'.inlets of
                Just outlet@(Outlet outlet' _ outletDataStream),
                Just inlet@(Inlet inlet' _ inletDataStream) ->
                    let
                        connectOutletMsg
                            = ConnectToOutlet outlet' outletDataStream
                            |> ChangeInlet inletId
                            |> ChangeNode srcNodeId
                        connectInletMsg
                            = ConnectToInlet inlet'
                            |> ChangeOutlet outletId
                            |> ChangeNode dstNodeId
                        link =
                            -- FIXME: do not repeat outletStream ?
                            Link srcNodeId dstNodeId outletId inletId outletDataStream
                        patchWithLink =
                            Patch
                                patch' { links = link : patch'.links }
                                patchSignal
                    in
                        patchWithLink
                              |> updatePatch connectOutletMsg
                              |> updatePatch connectInletMsg
                _, _ -> patch -- TODO: throw error
        _, _ -> patch -- TODO: throw error


disconnect
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
disconnect scrNodeId dstNodeId outletId inletId (Patch patch' patchSignal) =
    (Patch patch' patchSignal) -- FIXME: implement


-- helpers: Node

addInlet
    :: forall n c a x
     . c
    -> InletId
    -> String
    -> Node n c a x
    -> Node n c a x
addInlet type_ id label node@(Node node' nodeSignal processSignal) =
    let
        inletSignal = S.constant Bang
        inlet =
            Inlet
                { id : id
                , label : label
                , type : type_
                }
                (S.constant CreateInlet)
                inletSignal
    in
        Node
            node' { inlets = node'.inlets |> insert id inlet }
            nodeSignal
            processSignal

addOutlet
    :: forall n c a x
     . c
    -> OutletId
    -> String
    -> Node n c a x
    -> Node n c a x
addOutlet type_ id label node@(Node node' nodeSignal processSignal) =
    let
        outletSignal = S.constant Bang
        outlet =
            Outlet
                { id : id
                , label : label
                , type : type_
                }
                (S.constant CreateOutlet)
                outletSignal
    in
        Node
            node' { outlets = node'.outlets |> insert id outlet }
            nodeSignal
            processSignal



adaptPatchSignal :: forall n c a x. Patch n c a x -> MsgSignal (NetworkMsg n c a x)
adaptPatchSignal (Patch patch' patchSignal) =
    patchSignal S.~> (\patchMsg -> ChangePatch patch'.id patchMsg)


adaptNodeSignal :: forall n c a x. Node n c a x -> MsgSignal (PatchMsg n c a x)
adaptNodeSignal (Node node' nodeSignal _) =
    nodeSignal S.~> (\nodeMsg -> ChangeNode node'.id nodeMsg)


adaptInletSignal :: forall n c a x. Inlet c a x -> MsgSignal (NodeMsg c a x)
adaptInletSignal (Inlet inlet' inletSignal _) =
    inletSignal S.~> (\inletMsg -> ChangeInlet inlet'.id inletMsg)


adaptOutletSignal :: forall n c a x. Outlet c a x -> MsgSignal (NodeMsg c a x)
adaptOutletSignal (Outlet outlet' outletSignal _) =
    outletSignal S.~> (\outletMsg -> ChangeOutlet outlet'.id outletMsg)


tagFlowSignal :: forall n c a x. Inlet c a x -> TaggedFlowSignal a x
tagFlowSignal (Inlet inlet' _ flowSignal) =
    flowSignal S.~> (\val -> Tuple inlet'.id val)


-- make data items require a Show instance,
-- maybe even everywhere. Also create some type class which defines interfaces
-- for Node type and Channel type?
-- like accept() allow() etc.

log :: forall n c a x. Show a => Show x => Network n c a x -> S.Signal String
log = logNetwork


instance showNetworkMsg :: Show (NetworkMsg n c a x) where
    show CreateNetwork = "Create Network"
    show (AddPatch patchId title) = "Add Patch: " <> patchId <> " " <> title
    show (AddPatch' patchId) = "Add Patch: " <> patchId
    show (RemovePatch patchId) = "Remove Patch: " <> patchId
    show (SelectPatch patchId) = "Select Patch: " <> patchId
    show DeselectPatch = "Deselect Patch"
    show (EnterPatch patchId) = "Enter Patch: " <> patchId
    show (ExitPatch patchId) = "Exit Patch: " <> patchId
    show (ChangePatch patchId patchMsg) = "Change Patch: " <> patchId <> " :: " <> show patchMsg
    show (ChangePatch' (Patch patch' _)) = "Change Patch (ref): " <> patch'.id
    show NetworkGotEmpty = "Network got empty"


instance showPatchMsg :: Show (PatchMsg n c a x) where
    show CreatePatch = "Create Patch"
    show (AddNode _type nodeId title) = "Add Node: " <> nodeId <> " " <> title
    show (AddNode' _type nodeId) = "Add Node: " <> nodeId
    show (RemoveNode patchId) = "Remove Node: " <> patchId
    show (Connect srcNodeId dstNodeId outletId inletId) =
        "Connect Oulet " <> outletId <> " from Node " <> srcNodeId <>
        "to Inlet " <> inletId <> " from Node " <> dstNodeId
    show (Disconnect srcNodeId dstNodeId outletId inletId) =
        "Disconnect Oulet " <> outletId <> " from Node " <> srcNodeId <>
        "from Inlet " <> inletId <> " from Node " <> dstNodeId
    show (ChangeNode nodeId nodeMsg) =
        "Change Node: " <> nodeId <> " :: " <> show nodeMsg
    show (ChangeNode' (Node node' _ _)) = "Change Node (ref): " <> node'.id
    show PatchGotEmpty = "Patch got empty"


instance showNodeMsg :: Show (NodeMsg c a x) where
    show CreateNode = "Create Node"
    show (AddInlet type_ inletId title) = "Add Inlet: " <> inletId <> " " <> title
    show (AddInlet' type_ inletId) = "Add Inlet: " <> inletId
    show (AddOutlet type_ outletId title) = "Add Outlet: " <> outletId <> " " <> title
    show (AddOutlet' type_ outletId) = "Add Outlet: " <> outletId
    show (RemoveInlet inletId) = "Remove Inlet" <> inletId
    show (RemoveOutlet outletId) = "Remove Outlet" <> outletId
    show (ChangeInlet inletId inletMsg) =
        "Change Inlet: " <> inletId <> " :: " <> show inletMsg
    show (ChangeInlet' (Inlet inlet' _ _)) =
        "Change Inlet (ref): " <> inlet'.id
    show (ChangeOutlet outletId outletMsg) =
        "Change Outlet: " <> outletId <> " :: " <> show outletMsg
    show (ChangeOutlet' (Outlet outlet' _ _)) =
        "Change Outlet (ref): " <> outlet'.id
    show NodeGotEmpty = "Node got empty"


instance showInletMsg :: Show (InletMsg c a x) where
    show CreateInlet = "Create Inlet"
    show (ConnectToOutlet _ _) = "Connect to Outlet"
    show (DisconnectFromOutlet _) = "Disconnect from Outlet"
    show Hide = "Hide Inlet"


instance showOutletMsg :: Show (OutletMsg c a x) where
    show CreateOutlet = "Create Outlet"
    show (ConnectToInlet _) = "Connect to Inlet"
    show (DisconnectFromInlet _) = "Disconnect from Inlet"


logNetwork :: forall n c a x. Show a => Show x => Network n c a x -> S.Signal String
logNetwork (Network _ networkSignal) =
    networkSignal S.~> show



-- logData :: forall n c a x. Show a => Show x => Network n c a x -> S.Signal String
-- logData (Network _ networkSignal) =
--     networkSignal S.~> (\message ->
--         case message of
--             Bang -> show "Bang"
--             Data d -> show d
--             Error x -> show ("Error: " <> (show x)))
