module Rpd
    ( Id, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , App, Network, Patch, Node, Inlet, Outlet, Link
    , run
    , network, patch, node, inlet, outlet
    , addPatch, removePatch, select, deselect, enter, exit
    , addNode, addInlet, addOutlet
    -- , connect, disconnect
    -- , log--, logData
    ) where

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Prelude

import Control.Monad.Eff (Eff)
import Control.Plus (empty)
import Data.Array as Array
import Data.Function (apply, applyFlipped)
import Data.List ((:))
import Data.List as List
import Data.Map (Map, insert, delete, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Unit as Unit
import Signal as S
import Signal.Channel as SC

-- Elm-style operators

infixr 0 apply as <|
infixl 1 applyFlipped as |>

type Id = String


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
    = Start
    | UpdatePatch (Array PatchMsg) (Patch n c a x)
    | ForgetPatch PatchId
    | UpdateNode (Array (NodeMsg n)) (Node n c a x)
    | ForgetNode NodeId
    | UpdateInlet (Array (InletMsg c a x)) (Inlet c a x)
    | ForgetInlet InletId
    | UpdateOutlet (Array (OutletMsg c a x)) (Outlet c a x)
    | ForgetOutlet OutletId
    | Connect NodeId NodeId OutletId InletId
    | Disconnect NodeId NodeId OutletId InletId
    | Stop


data PatchMsg
    = InitPatch String
    | SelectPatch
    | DeselectPatch
    | EnterPatch
    | ExitPatch
    | AddNode NodeId
    | RemoveNode NodeId


data NodeMsg n
    = InitNode n String
    | AddInlet InletId
    | RemoveInlet InletId
    | AddOutlet InletId
    | RemoveOutlet OutletId


data InletMsg c a x
    = InitInlet c String
    | ConnectToOutlet OutletId (FlowSignal a x)
    | DisconnectFromOutlet OutletId
    | HideInlet
    | RevealInlet


data OutletMsg c
    = InitOutlet c String
    | ConnectToInlet InletId
    | DisconnectFromInlet InletId


data Value a x
    = Bang
    | Data a
    | Error x
    | SysError String


-- type Actions n c a x = List.List (Action n c a x)


-- init = ( [] )

-- addNode :: NodeActions -> PatchActions

-- addInlet :: InletActions -> NodeActions

-- modifyInlet :: InletActions -> InletActions

-- ^ WRITER MONAD

-- etc...

-- run :: Actions n c a x ->


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


data Network n c a x =
    Network
        { messages :: S.Signal (NetworkMsg n c a x)
        , patches :: Map PatchId (Patch n c a x)
        , nodes :: Map NodeId (Node n c a x)
        , inlets :: Map InletId (Inlet c a x)
        , outlets :: Map OutletId (Outlet c a x)
        , links ::  Map LinkId (Link c a x)
        , selected :: Maybe PatchId
        , entered :: Array PatchId
        }


data Patch n c a x =
    Patch
        { id :: PatchId
        , title :: String
        , nodes :: Array NodeId
        , links :: Array LinkId
        }


data Node n c a x =
    Node
        { id :: NodeId
        , title :: String
        , type :: Maybe n
        , inlets :: Array InletId
        , outlets :: Array OutletId
        , process :: Maybe (Map InletId (Value a x) -> Map OutletId (Value a x))
        }
        (ProcessSignal a x)


data Inlet c a x =
    Inlet
        { id :: InletId
        , label :: String
        , type :: Maybe c
        }
        (FlowSignal a x)


data Outlet c a x =
    Outlet
        { id :: OutletId
        , label :: String
        , type :: Maybe c
        }
        (FlowSignal a x)


data Link a x = Link
        { id :: LinkId
        , inlet :: InletId
        , outlet :: OutletId
        }
        (FlowSignal a x)


type NetworkActions' n c a x = Writer (Array (NetworkMsg n c a x)) (Network n c a x)
type PatchActions' n c a x = Writer (Array (PatchMsg n c a x)) (Patch n c a x)
type NodeActions' n c a x = Writer (Array (NodeMsg n c a x)) (Node n c a x)
type InletActions' c a x = Writer (Array (InletMsg c a x)) (Inlet c a x)
type OutletActions' c a x = Writer (Array (OutletMsg c a x)) (Outlet c a x)
type LinkActions' c a x = (Link c a x)


data Actions n c a x
    = NetworkActions (NetworkActions' n c a x)
    | PatchActions (PatchActions' n c a x)
    | NodeActions (NodeActions' n c a x)
    | InletActions (InletActions' c a x)
    | OutletActions (OutletActions' c a x)
    | LinkActions (LinkActions' c a x)


-- API:


network' :: Network
network' =
    Network
        { messages : S.constant Start
        , patches : Map.empty
        , nodes : Map.empty
        , inlets : Map.empty
        , outlets : Map.empty
        , links :  Map.empty
        , selected : Nothing
        , entered : []
        }


patch' :: Patch
patch' =
    Patch
        { id : "test"
        , title : "Noname"
        , nodes : []
        , links : []
        }


node' :: Node
node' =
    Node
        { id : "test"
        , title : "Unknown"
        , type : Nothing
        , inlets : []
        , outlets : []
        , process : Nothing
        }


inlet' :: Inlet
inlet' =
    Inlet
        { id: "test"
        , label : "foo"
        , type : Nothing
        }
        S.constant Bang


outlet' :: Outlet
outlet' =
    Outlet
        { id: "test"
        , label : "foo"
        , type : Nothing
        }
        S.constant Bang


tellAndPerform :: forall a b. b -> (b -> a) -> a -> Writer (Array b) a
tellAndPerform msg updateF subj = do
    tell [ msg ]
    updateF msg subj


tellAndPerform' :: forall a b. b -> (b -> a) -> Writer (Array b) a -> Writer (Array b) a
tellAndPerform' msg updateF (WriterT prevMsgs subj) = do
    tell (msg : prevMsgs)
    updateF msg subj


network :: NetworkActions'
network = tellAndPerform Start update network'


patch :: String -> PatchActions'
patch title =
    tellAndPerform (InitPatch title) updatePatch patch'


node :: forall n. n -> String -> NodeActions'
node type_ title =
    tellAndPerform (InitNode type_ title) updateNode node'


inlet :: forall c. c -> String -> NodeActions'
inlet type_ label =
    tellAndPerform (InitInlet type_ label) updateInlet inlet'


outlet :: forall c. c -> String -> NodeActions'
outlet type_ label =
    tellAndPerform (InitOutlet type_ label) updateOutlet outlet'


addPatch :: PatchActions' -> NetworkActions' -> NetworkActions'
addPatch (WriterT patchActions patch) network =
    tellAndPerform' (UpdatePatch patchActions patch) update network


removePatch :: PatchActions' -> NetworkActions'
removePatch (WriterT _ patch) network =
    -- FIXME: use unperformed patch actions?
    tellAndPerform' (ForgetPatch patch.id) update network


select :: PatchActions' -> PatchActions'
select patch =
    tellAndPerform' SelectPatch updatePatch patch


deselect :: PatchActions' -> PatchActions'
deselect patch =
    tellAndPerform' DeselectPatch updatePatch patch


enter :: PatchActions' -> PatchActions'
enter patch =
    tellAndPerform' EnterPatch updatePatch patch


exit :: PatchActions' -> PatchActions'
exit patch =
    tellAndPerform' ExitPatch updatePatch patch


addNode :: NodeActions' -> PatchActions' -> PatchActions'
addNode (WriterT nodeActions node) patch =
    tellAndPerform' (UpdateNode nodeActions node) updatePatch patch


removeNode :: NodeActions' -> PatchActions' -> PatchActions'
removeNode (WriterT _ node) patch =
    -- FIXME: use unperformed inlet actions?
    tellAndPerform' (ForgetInlet inlet.id) updateNode node


addInlet :: InletActions' -> NodeActions' -> NodeActions'
addInlet (WriterT inletActions inlet) node =
    tellAndPerform' (UpdateInlet inletActions inlet) updateNode node


removeInlet :: InletActions' -> NodeActions' -> NodeActions'
removeInlet (WriterT _ inlet) node =
    -- FIXME: use unperformed inlet actions?
    tellAndPerform' (ForgetInlet inlet.id) updateNode node


addOutlet :: OutletActions' -> NodeActions' -> NodeActions'
addOutlet (WriterT outletActions outlet) node =
    tellAndPerform' (UpdateOutlet outletActions outlet) updateNode node


removeOutlet :: OutletActions' -> NodeActions' -> NodeActions'
removeOutlet (WriterT _ outlet) node =
    -- FIXME: use unperformed outlet actions?
    tellAndPerform' (ForgetOutlet outlet.id) updateNode node


-- Logic:

type Renderer n c a x eff = (Network n c a x -> Eff eff Unit)


data App nodes channels datatype error effect =
    App
        { network :: Maybe (Network nodes channels datatype error)
        -- , data :: SC.Channel
        --     { patch :: PatchId
        --     , node :: NodeId
        --     , inlet :: InletId
        --     , value :: Value datatype error
        --     }
        -- , data :: ProcessChannel datatype error
        , renderers :: Array (Renderer nodes channels datatype error effect)
        }


run :: forall n c a x eff
     . Array (NetworkMsg n c a x)
    -> (S.Signal (NetworkMsg n c a x) ->
        S.Signal (Eff (channel :: SC.CHANNEL | eff) Unit))
    -> Eff (channel :: SC.CHANNEL | eff) Unit
run messages f = void do
    c <- SC.channel Start
    let s = SC.subscribe c
    SC.send c (Array.head messages |> fromMaybe Stop)
    S.runSignal (f s)
    Array.foldM
        (\msgStack msg -> do
            SC.send c msg
            --fromMaybe [] (tail msgStack)
        )
        Unit.unit
        (Array.tail messages |> fromMaybe [ Stop ])
    pure s


initProcessChannel :: forall a x. ProcessSignal a x
initProcessChannel =
    S.constant (Tuple Map.empty Map.empty)


update :: forall n c a x. NetworkMsg n c a x -> Network n c a x -> Network n c a x
update Start network               = network



-- update (AddPatch id title) network = network |> addPatch' id title
-- update (AddPatch' id) network      = network |> addPatch' id id
-- update (RemovePatch id) network    = network |> removePatch' id
-- update (SelectPatch id) network    = network |> selectPatch' id
-- update DeselectPatch network       = network |> deselectPatch'
-- update (EnterPatch id) network     = network |> enterPatch' id
-- update (ExitPatch id) network      = network |> exitPatch' id
-- --update Stop network                = network
-- update (ChangePatch patchId patchMsg) network@(Network network') =
--     case network'.patches |> Map.lookup patchId of
--         Just patch ->
--             update (ChangePatch' (patch |> updatePatch patchMsg)) network
--         Nothing -> network -- TODO: throw error
-- update (ChangePatch' updatedPatch@(Patch patch')) network@(Network network') =
--     Network
--         network' { patches =
--             network'.patches |> Map.insert patch'.id updatedPatch }
-- update Stop network                = network


updatePatch :: forall n c a x. PatchMsg n c a x -> Patch n c a x -> Patch n c a x
updatePatch (InitPatch title) patch              = patch

-- updatePatch (AddNode type_ id title) patch = patch |> addNode' type_ id title
-- updatePatch (AddNode' type_ id) patch      = patch |> addNode' type_ id id
-- updatePatch (RemoveNode id) patch          = patch |> removeNode' id
-- updatePatch (Connect srcNodeId dstNodeId inletId outletId) patch =
--     patch |> connect' srcNodeId dstNodeId inletId outletId
-- updatePatch (Disconnect srcNodeId dstNodeId inletId outletId) patch =
--     patch |> disconnect' srcNodeId dstNodeId inletId outletId
-- updatePatch (ChangeNode nodeId nodeMsg) patch@(Patch patch') =
--     case patch'.nodes |> Map.lookup nodeId of
--         Just node ->
--             updatePatch (ChangeNode' (node |> updateNode nodeMsg)) patch
--         Nothing -> patch -- TODO: throw error
-- updatePatch (ChangeNode' updatedNode@(Node node' _)) (Patch patch') =
--     Patch
--         patch'
--             { nodes = patch'.nodes |> Map.insert node'.id updatedNode
--             }


updateNode :: forall n c a x. NodeMsg c a x -> Node n c a x -> Node n c a x
updateNode (InitNode title) node              = node

-- updateNode CreateNode node                 = node
-- updateNode (AddInlet type_ id title) node  = node |> addInlet' type_ id title
-- updateNode (AddInlet' type_ id) node       = node |> addInlet' type_ id id
-- updateNode (AddOutlet type_ id title) node = node |> addOutlet' type_ id title
-- updateNode (AddOutlet' type_ id) node      = node |> addInlet' type_ id id
-- updateNode (RemoveInlet id) node           = node -- |> removeInlet id
-- updateNode (RemoveOutlet id) node          = node -- |> removeOutlet id
-- updateNode (ChangeInlet inletId inletMsg) node@(Node node' processSignal) =
--     case node'.inlets |> Map.lookup inletId of
--         Just inlet ->
--             updateNode (ChangeInlet' (inlet |> updateInlet inletMsg)) node
--         Nothing -> node -- TODO: throw error
-- updateNode (ChangeInlet' updatedInlet@(Inlet inlet' _)) node@(Node node' _) =
--     let
--         inlets' = node'.inlets |> Map.insert inlet'.id updatedInlet
--         processSignal' =
--             case node'.process of
--                 Just processFn ->
--                     case S.mergeMany (map tagFlowSignal inlets') of
--                         Just taggedSignal ->
--                             S.foldp (\(Tuple inletId inletVal) (Tuple inletVals _) ->
--                                 let
--                                     inletVals' = inletVals |> Map.insert inletId inletVal
--                                     outletVals' = processFn inletVals'
--                                 in
--                                     Tuple inletVals' outletVals'
--                             ) (Tuple Map.empty Map.empty) taggedSignal
--                         Nothing -> initProcessChannel
--                 Nothing -> initProcessChannel -- processChannel
--         outlets' = map (\(Outlet outlet' flowSignal) ->
--             let
--                 flowSignal' =
--                     S.merge
--                         (S.filterMap
--                             (\(Tuple _ outletVals) -> Map.lookup outlet'.id outletVals)
--                             (SysError "Failed to find outlet")
--                             processSignal')
--                         flowSignal
--             in
--                 Outlet outlet' flowSignal'
--         ) node'.outlets
--     in
--         Node
--             node' { inlets = inlets' }
--             processSignal'
-- updateNode (ChangeOutlet outletId outletMsg) node@(Node node' processSignal) =
--     case node'.outlets |> Map.lookup outletId of
--         Just outlet ->
--             updateNode (ChangeOutlet' (outlet |> updateOutlet outletMsg)) node
--         Nothing -> node -- TODO: throw error
-- updateNode (ChangeOutlet' updatedOutlet@(Outlet outlet' _)) node@(Node node' processSignal) =
--     Node
--         node'
--             { outlets = node'.outlets |> Map.insert outlet'.id updatedOutlet
--             }
--         processSignal


updateInlet :: forall c a x. InletMsg c a x -> Inlet c a x -> Inlet c a x
updateInlet (InitInlet label) = inlet
-- updateInlet CreateInlet inlet = inlet
-- updateInlet (ConnectToOutlet _ signal) (Inlet inlet' dataSignal) =
--     Inlet inlet' (S.merge dataSignal signal)
-- updateInlet (DisconnectFromOutlet _) inlet = inlet -- FIXME: implement
-- updateInlet Hide inlet = inlet


updateOutlet :: forall c a x. OutletMsg c a x -> Outlet c a x -> Outlet c a x
updateOutlet (InitInlet label) outlet = outlet
-- updateOutlet (ConnectToInlet _) outlet = outlet
-- updateOutlet (DisconnectFromInlet _) outlet = outlet -- FIXME: implement


-- Send, Attach etc.

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


instance showNetwork :: Show (Network n c a x) where
    show network = "(Network \n"
        <> Map.size network.patches <> " Patches\n"
        <> Map.size network.nodes <> " Nodes\n"
        <> Map.size network.inlets <> " Inlets\n"
        <> Map.size network.outlets <> " Outlets\n"
        <> "Selected Patch: " <> show (lift network.selected)
        <> "Entered Patches: " <> network.entered
        <> ")"


instance showPatch :: Show (Patch n c a x) where
    show patch = "(Patch " <> patch.id <> "\n"
        <> Array.length patch.nodes <> " Nodes\n"
        <> Array.length patch.links <> " Links\n"
        <> ")"


instance showNode :: Show (Node n c a x) where
    show node = "(Node " <> node.id <> "\n"
        <> Array.length node.inlets <> " Inlets\n"
        <> Array.length node.outlets <> " Outlets\n"
        <> ")"


instance showInlet :: Show (Inlet c a x) where
    show inlet = "(Inlet " <> inlet.id <> " " <> inlet.label <> ")"


instance showOutlet :: Show (Outlet c a x) where
    show outlet = "(Outlet " <> outlet.id <> " " <> outlet.label <> ")"


instance showNetworkMsg :: Show (NetworkMsg n c a x) where
    show Start = "Start"
    show (UpdatePatch _ patch) = "Update patch: " <> show patch
    show (ForgetPatch patchId) = "Forget patch: " <> patchId
    show (UpdateNode _ node) = "Update node: " <> show node
    show (ForgetNode nodeId) = "Forget node: " <> nodeId
    show (UpdateInlet _ inlet) = "Update inlet: " <> show inlet
    show (ForgetInlet inletId) = "Forget inlet: " <> inletId
    show (UpdateOutlet _ outlet) = "Update outlet: " <> show outlet
    show (ForgetOutlet outletId) = "Forget outlet: " <> outletId
    show (Connect srcNodeId dstNodeId outletId inletId) = "Connect:\n"
        <> "Node " <> srcNodeId <> " -> Node " <> dstNodeId <> "\n"
        <> "Outlet " <> outletId <> " -> Inlet " <> inletId
    show (Disconnect srcNodeId dstNodeId outletId inletId) = "Disconnect:\n"
        <> "Node " <> srcNodeId <> " -> Node " <> dstNodeId <> "\n"
        <> "Outlet " <> outletId <> " -> Inlet " <> inletId
    show Stop = "Stop"


instance showPatchMsg :: Show (PatchMsg n c a x) where
    show (InitPatch title) = "Init patch: " <> title
    show SelectPatch = "Select patch"
    show DeselectPatch = "Deselect patch"
    show EnterPatch = "Enter patch"
    show ExitPatch = "Exit patch"
    show (AddNode nodeId) = "Add node: " <> nodeId
    show (RemoveNode nodeId) = "Remove node:"  <> nodeId



instance showNodeMsg :: Show (NodeMsg n c a x) where
    show (InitNode type_ title) = "Init node: " <> type_ <> " " <> title
    show (AddInlet inletId) = "Add inlet: " <> inletId
    show (RemoveInlet inletId) = "Remove inlet:"  <> inletId
    show (AddOutlet outletId) = "Add outlet: " <> outletId
    show (RemoveOutlet outletId) = "Remove outlet:"  <> outletId


instance showInletMsg :: Show (InletMsg c a x) where
    show (InitInlet type_ label) = "Init inlet: " <> type_ <> " " <> label
    show (ConnectToOutlet outletId _) = "Connect to outlet: " <> outletId
    show (DisconnectFromOutlet outletId) = "Disconnect from outlet:"  <> outletId
    show HideInlet = "Hide inlet"
    show RevealInlet = "Reveal inlet"


instance showOutletMsg :: Show (OutletMsg c a x) where
    show (InitOutlet type_ label) = "Init outlet: " <> type_ <> " " <> label
    show (ConnectToInlet inletId) = "Connect to inlet: " <> inletId
    show (DisconnectFromInlet inletId) = "Disconnect from inlet:"  <> inletId


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
