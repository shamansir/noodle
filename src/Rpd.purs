module Rpd
    ( Id, NetworkId, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , Network, Patch, Node, Inlet, Outlet, Link
    -- exported only for tests, remove it later
    , FlowSignal, Value, Outlet', Inlet', Node', Patch', Network'
    , NetworkMsg(..), PatchMsg(..), NodeMsg(..), InletMsg(..), OutletMsg(..)
    -- end of the things to remove
    , update, init
    , addPatch, removePatch, selectPatch, deselectPatch, enterPatch, exitPatch
    , addNode, addInlet, addOutlet, connect, disconnect
    -- , log--, logData
    ) where

import Prelude

import Control.Plus (empty)
import Control.Monad.Eff (Eff)

import Data.Array ((:))
import Data.Array as Array
import Data.Function (apply, applyFlipped)
import Data.List as List
import Data.Map (Map, insert, delete, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Signal as S
import Signal.Channel as SC

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
    = CreateNetwork NetworkId
    | AddPatch PatchId String
    | AddPatch' PatchId
    | RemovePatch PatchId
    | SelectPatch PatchId
    | DeselectPatch
    | EnterPatch PatchId
    | ExitPatch PatchId
    | ChangePatch PatchId (PatchMsg n c a x)
    | ChangePatch' (Patch n c a x)
    | Stop


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


-- data Render
--     = Html
--     | Svg
--     | Console


type Renderer n c a x eff = (Network n c a x -> Eff eff Unit)


type App nodes channels datatype error effect =
    { network :: Network nodes channels datatype error
    , messages :: S.Signal (NetworkMsg nodes channels datatype error)
    -- , data :: SC.Channel
    --     { patch :: PatchId
    --     , node :: NodeId
    --     , inlet :: InletId
    --     , value :: Value datatype error
    --     }
    -- , data :: ProcessChannel datatype error
    , renderers :: Array (Renderer nodes channels datatype error effect)
    }


-- The signal where all the data flows: Bangs, data chunks and errors
-- type FlowChannel a x = SC.Channel (Value a x)
type FlowSignal a x = S.Signal (Value a x)

-- type TaggedFlowChannel a x = SC.Channel (Tuple InletId (Value a x))
type TaggedFlowSignal a x = S.Signal (Tuple InletId (Value a x))

-- The signal where the messages go
--type MsgChannel m = SC.Channel m
-- type MsgEmitter eff m = SL.Emitter eff m

-- The special channel for nodes which tracks the data flow through node inputs and outlets
-- type ProcessChannel a x = SC.Channel (Tuple (Map InletId (Value a x)) (Map OutletId (Value a x)))
type ProcessSignal a x = S.Signal (Tuple (Map InletId (Value a x)) (Map OutletId (Value a x)))

data Network n c a x = Network (Network' n c a x)

data Patch n c a x = Patch (Patch' n c a x)

data Node n c a x = Node (Node' n c a x) (ProcessSignal a x)

data Inlet c a x = Inlet (Inlet' c) (FlowSignal a x)

data Outlet c a x = Outlet (Outlet' c) (FlowSignal a x)

data Link c a x = Link NodeId NodeId OutletId InletId (FlowSignal a x)

initProcessChannel :: forall a x. ProcessSignal a x
initProcessChannel =
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


update :: forall n c a x. NetworkMsg n c a x -> Network n c a x -> Network n c a x
update (CreateNetwork id) network  = network
update (AddPatch id title) network = network |> addPatch' id title
update (AddPatch' id) network      = network |> addPatch' id id
update (RemovePatch id) network    = network |> removePatch' id
update (SelectPatch id) network    = network |> selectPatch' id
update DeselectPatch network       = network |> deselectPatch'
update (EnterPatch id) network     = network |> enterPatch' id
update (ExitPatch id) network      = network |> exitPatch' id
update Stop network                = network
update (ChangePatch patchId patchMsg) network@(Network network') =
    case network'.patches |> Map.lookup patchId of
        Just patch ->
            update (ChangePatch' (patch |> updatePatch patchMsg)) network
        Nothing -> network -- TODO: throw error
update (ChangePatch' updatedPatch@(Patch patch')) network@(Network network') =
    Network
        network' { patches =
            network'.patches |> Map.insert patch'.id updatedPatch }


updatePatch :: forall n c a x. PatchMsg n c a x -> Patch n c a x -> Patch n c a x
updatePatch CreatePatch patch              = patch
updatePatch (AddNode type_ id title) patch = patch |> addNode' type_ id title
updatePatch (AddNode' type_ id) patch      = patch |> addNode' type_ id id
updatePatch (RemoveNode id) patch          = patch |> removeNode' id
updatePatch (Connect srcNodeId dstNodeId inletId outletId) patch =
    patch |> connect' srcNodeId dstNodeId inletId outletId
updatePatch (Disconnect srcNodeId dstNodeId inletId outletId) patch =
    patch |> disconnect' srcNodeId dstNodeId inletId outletId
updatePatch (ChangeNode nodeId nodeMsg) patch@(Patch patch') =
    case patch'.nodes |> Map.lookup nodeId of
        Just node ->
            updatePatch (ChangeNode' (node |> updateNode nodeMsg)) patch
        Nothing -> patch -- TODO: throw error
updatePatch (ChangeNode' updatedNode@(Node node' _)) (Patch patch') =
    Patch
        patch'
            { nodes = patch'.nodes |> Map.insert node'.id updatedNode
            }


updateNode :: forall n c a x. NodeMsg c a x -> Node n c a x -> Node n c a x
updateNode CreateNode node                 = node
updateNode (AddInlet type_ id title) node  = node |> addInlet' type_ id title
updateNode (AddInlet' type_ id) node       = node |> addInlet' type_ id id
updateNode (AddOutlet type_ id title) node = node |> addOutlet' type_ id title
updateNode (AddOutlet' type_ id) node      = node |> addInlet' type_ id id
updateNode (RemoveInlet id) node           = node -- |> removeInlet id
updateNode (RemoveOutlet id) node          = node -- |> removeOutlet id
updateNode (ChangeInlet inletId inletMsg) node@(Node node' processSignal) =
    case node'.inlets |> Map.lookup inletId of
        Just inlet ->
            updateNode (ChangeInlet' (inlet |> updateInlet inletMsg)) node
        Nothing -> node -- TODO: throw error
updateNode (ChangeInlet' updatedInlet@(Inlet inlet' _)) node@(Node node' _) =
    let
        inlets' = node'.inlets |> Map.insert inlet'.id updatedInlet
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
                        Nothing -> initProcessChannel
                Nothing -> initProcessChannel -- processChannel
        outlets' = map (\(Outlet outlet' flowSignal) ->
            let
                flowSignal' =
                    S.merge
                        (S.filterMap
                            (\(Tuple _ outletVals) -> Map.lookup outlet'.id outletVals)
                            (SysError "Failed to find outlet")
                            processSignal')
                        flowSignal
            in
                Outlet outlet' flowSignal'
        ) node'.outlets
    in
        Node
            node' { inlets = inlets' }
            processSignal'
updateNode (ChangeOutlet outletId outletMsg) node@(Node node' processSignal) =
    case node'.outlets |> Map.lookup outletId of
        Just outlet ->
            updateNode (ChangeOutlet' (outlet |> updateOutlet outletMsg)) node
        Nothing -> node -- TODO: throw error
updateNode (ChangeOutlet' updatedOutlet@(Outlet outlet' _)) node@(Node node' processSignal) =
    Node
        node'
            { outlets = node'.outlets |> Map.insert outlet'.id updatedOutlet
            }
        processSignal


updateInlet :: forall c a x. InletMsg c a x -> Inlet c a x -> Inlet c a x
updateInlet CreateInlet inlet = inlet
updateInlet (ConnectToOutlet _ signal) (Inlet inlet' dataSignal) =
    Inlet inlet' (S.merge dataSignal signal)
updateInlet (DisconnectFromOutlet _) inlet = inlet -- FIXME: implement
updateInlet Hide inlet = inlet


updateOutlet :: forall c a x. OutletMsg c a x -> Outlet c a x -> Outlet c a x
updateOutlet CreateOutlet outlet = outlet
updateOutlet (ConnectToInlet _) outlet = outlet
updateOutlet (DisconnectFromInlet _) outlet = outlet -- FIXME: implement


-- Send, Attach etc.


-- API:

addPatch :: forall n c a x. PatchId -> String -> Network n c a x -> Network n c a x
addPatch patchId title network = network |> update (AddPatch patchId title)

removePatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
removePatch patchId network = network |> update (RemovePatch patchId)

selectPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
selectPatch patchId network = network |> update (SelectPatch patchId)

deselectPatch :: forall n c a x. Network n c a x -> Network n c a x
deselectPatch network = network |> update DeselectPatch

enterPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
enterPatch patchId network = network |> update (EnterPatch patchId)

exitPatch :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
exitPatch patchId network = network |> update (ExitPatch patchId)

addNode :: forall n c a x. n -> NodeId -> String -> Patch n c a x -> Patch n c a x
addNode type_ nodeId title patch = patch |> updatePatch (AddNode type_ nodeId title)

removeNode :: forall n c a x. NodeId -> Patch n c a x -> Patch n c a x
removeNode nodeId patch = patch |> updatePatch (RemoveNode nodeId)


connect
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
connect srcNodeId dstNodeId outletId inletId patch =
    patch |> updatePatch (Connect srcNodeId dstNodeId outletId inletId)

disconnect
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
disconnect srcNodeId dstNodeId outletId inletId patch =
    patch |> updatePatch (Disconnect srcNodeId dstNodeId outletId inletId)

addInlet
    :: forall n c a x
     . c
    -> InletId
    -> String
    -> Node n c a x
    -> Node n c a x
addInlet type_ inletId label node =
    node |> updateNode (AddInlet type_ inletId label)

addOutlet
    :: forall n c a x
     . c
    -> OutletId
    -> String
    -> Node n c a x
    -> Node n c a x
addOutlet type_ inletId label node =
    node |> updateNode (AddOutlet type_ inletId label)

-- implementations: Network

addPatch' :: forall n c a x. PatchId -> String -> Network n c a x -> Network n c a x
addPatch' id title (Network network') =
    let
        patch@(Patch patch') =
            Patch
                { id : id
                , title : title
                , nodes : Map.empty
                , links : []
                }
    in
        Network
            network'
                { patches = network'.patches |> insert patch'.id patch
                }


removePatch' :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
removePatch' patchId (Network network') =
    Network
        network'
            { patches = network'.patches |> delete patchId
            }


selectPatch' :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
selectPatch' id (Network network') =
    Network
        network' { selected = Just id }

deselectPatch' :: forall n c a x. Network n c a x -> Network n c a x
deselectPatch' (Network network') =
    Network
        network' { selected = Nothing }


enterPatch' :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
enterPatch' id (Network network') =
    Network
        network' { entered = id : network'.entered }


exitPatch' :: forall n c a x. PatchId -> Network n c a x -> Network n c a x
exitPatch' id (Network network') =
    Network
        network' { entered = Array.delete id network'.entered }


-- implementations: Patch

addNode' :: forall n c a x. n -> NodeId -> String -> Patch n c a x -> Patch n c a x
addNode' type_ id title (Patch patch') =
    let
        node@(Node node' _) =
            Node
                { id : id
                , title : title
                , type : type_
                , process : Nothing
                , inlets : Map.empty
                , outlets : Map.empty
                }
                (S.constant (Tuple Map.empty Map.empty))
    in
        Patch
            patch'
                { nodes = patch'.nodes |> insert node'.id node
                }


removeNode' :: forall n c a x. NodeId -> Patch n c a x -> Patch n c a x
removeNode' nodeId (Patch patch') =
    Patch
        patch' { nodes = patch'.nodes |> delete nodeId }


connect'
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
connect' srcNodeId dstNodeId outletId inletId patch@(Patch patch') =
    case Map.lookup srcNodeId patch'.nodes,
         Map.lookup dstNodeId patch'.nodes of
        Just srcNode@(Node srcNode' _),
        Just dstNode@(Node dstNode' _) ->
            case Map.lookup outletId srcNode'.outlets,
                 Map.lookup inletId dstNode'.inlets of
                Just outlet@(Outlet outlet' outletDataStream),
                Just inlet@(Inlet inlet' inletDataStream) ->
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
                    in
                        patchWithLink
                              |> updatePatch connectOutletMsg
                              |> updatePatch connectInletMsg
                _, _ -> patch -- TODO: throw error
        _, _ -> patch -- TODO: throw error


disconnect'
    :: forall n c a x
     . NodeId
    -> NodeId
    -> OutletId
    -> InletId
    -> Patch n c a x
    -> Patch n c a x
disconnect' scrNodeId dstNodeId outletId inletId (Patch patch') =
    (Patch patch') -- FIXME: implement


-- implementations: Node

addInlet'
    :: forall n c a x
     . c
    -> InletId
    -> String
    -> Node n c a x
    -> Node n c a x
addInlet' type_ id label (Node node' processSignal) =
    let
        inletSignal = S.constant Bang
        inlet =
            Inlet
                { id : id
                , label : label
                , type : type_
                }
                inletSignal
    in
        Node
            node' { inlets = node'.inlets |> insert id inlet }
            processSignal

addOutlet'
    :: forall n c a x
     . c
    -> OutletId
    -> String
    -> Node n c a x
    -> Node n c a x
addOutlet' type_ id label node@(Node node' processSignal) =
    let
        outletSignal = S.constant Bang
        outlet =
            Outlet
                { id : id
                , label : label
                , type : type_
                }
                outletSignal
    in
        Node
            node' { outlets = node'.outlets |> insert id outlet }
            processSignal



-- adaptPatchSignal :: forall n c a x. Patch n c a x -> MsgChannel (NetworkMsg n c a x)
-- adaptPatchSignal (Patch patch' patchSignal) =
--     patchSignal S.~> (\patchMsg -> ChangePatch patch'.id patchMsg)


-- adaptNodeSignal :: forall n c a x. Node n c a x -> MsgChannel (PatchMsg n c a x)
-- adaptNodeSignal (Node node' nodeSignal _) =
--     nodeSignal S.~> (\nodeMsg -> ChangeNode node'.id nodeMsg)


-- adaptInletSignal :: forall n c a x. Inlet c a x -> MsgChannel (NodeMsg c a x)
-- adaptInletSignal (Inlet inlet' inletSignal _) =
--     inletSignal S.~> (\inletMsg -> ChangeInlet inlet'.id inletMsg)


-- adaptOutletSignal :: forall n c a x. Outlet c a x -> MsgChannel (NodeMsg c a x)
-- adaptOutletSignal (Outlet outlet' outletSignal _) =
--     outletSignal S.~> (\outletMsg -> ChangeOutlet outlet'.id outletMsg)


tagFlowSignal :: forall n c a x. Inlet c a x -> TaggedFlowSignal a x
tagFlowSignal (Inlet inlet' flowSignal) =
    flowSignal S.~> (\val -> Tuple inlet'.id val)


-- make data items require a Show instance,
-- maybe even everywhere. Also create some type class which defines interfaces
-- for Node type and Channel type?
-- like accept() allow() etc.


instance showNetworkMsg :: Show (NetworkMsg n c a x) where
    show (CreateNetwork networkId) = "Create Network: " <> networkId
    show (AddPatch patchId title) = "Add Patch: " <> patchId <> " " <> title
    show (AddPatch' patchId) = "Add Patch: " <> patchId
    show (RemovePatch patchId) = "Remove Patch: " <> patchId
    show (SelectPatch patchId) = "Select Patch: " <> patchId
    show DeselectPatch = "Deselect Patch"
    show (EnterPatch patchId) = "Enter Patch: " <> patchId
    show (ExitPatch patchId) = "Exit Patch: " <> patchId
    show (ChangePatch patchId patchMsg) = "Change Patch: " <> patchId <> " :: " <> show patchMsg
    show (ChangePatch' (Patch patch')) = "Change Patch (ref): " <> patch'.id
    show Stop = "Stop"


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
    show (ChangeNode' (Node node' _)) = "Change Node (ref): " <> node'.id


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
    show (ChangeInlet' (Inlet inlet' _)) =
        "Change Inlet (ref): " <> inlet'.id
    show (ChangeOutlet outletId outletMsg) =
        "Change Outlet: " <> outletId <> " :: " <> show outletMsg
    show (ChangeOutlet' (Outlet outlet' _)) =
        "Change Outlet (ref): " <> outlet'.id


instance showInletMsg :: Show (InletMsg c a x) where
    show CreateInlet = "Create Inlet"
    show (ConnectToOutlet _ _) = "Connect to Outlet"
    show (DisconnectFromOutlet _) = "Disconnect from Outlet"
    show Hide = "Hide Inlet"


instance showOutletMsg :: Show (OutletMsg c a x) where
    show CreateOutlet = "Create Outlet"
    show (ConnectToInlet _) = "Connect to Inlet"
    show (DisconnectFromInlet _) = "Disconnect from Inlet"


instance showValue :: ( Show a, Show x ) => Show (Value a x) where
    show Bang = "Bang"
    show (Data val) = "Data: " <> show val
    show (Error err) = "Error: " <> show err
    show (SysError msg) = "System Error: " <> msg


-- log :: forall n c a x. Network n c a x -> S.Signal String
-- log = logNetwork


-- logNetwork :: forall n c a x. Network n c a x -> S.Signal String
-- logNetwork (Network _ networkSignal) =
--     networkSignal S.~> show


logDataFlow :: forall n c a x. Show a => Show x => Network n c a x -> S.Signal String
logDataFlow (Network network') =
    let
        allNodes = List.foldr
            (\(Patch patch') allNodes ->
                allNodes <> values patch'.nodes) empty (values network'.patches)
        allDataSignals =
            List.foldr (\(Node node' dataSignal) allSignals ->
                allSignals
                    <> (map (\(Inlet inlet' dataSignal) -> dataSignal) (values node'.inlets))
                    <> (map (\(Outlet outlet' dataSignal) -> dataSignal) (values node'.outlets))
            ) empty allNodes
    in
        case S.mergeMany allDataSignals of
            Just signal -> map show signal
            Nothing -> S.constant "Empty"
