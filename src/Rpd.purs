module Rpd
    ( Id, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , App, Network, Patch, Node, Inlet, Outlet, Link
    -- TRY TO REMOVE LATER
    , NetworkMsg(..), PatchMsg(..), NodeMsg(..), InletMsg(..), OutletMsg(..)
    , FlowSignal, Value, Actions', TaggedActions'
    -- END OF TRY TO REMOVE LATER
    , run
    , network, patch, node, inlet, outlet
    , addPatch, removePatch, select, deselect, enter, exit
    , addNode, addInlet, addOutlet
    -- , connect, disconnect
    -- , log--, logData
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Array
import Data.Function (apply, applyFlipped)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
-- `e` — effect

data NetworkMsg n c a x
    = Start
    | AddPatch PatchId
    | UpdatePatch PatchId (PatchMsg n c a x)
    | ForgetPatch PatchId
    | Stop


data PatchMsg n c a x
    = InitPatch String
    | SelectPatch
    | DeselectPatch
    | EnterPatch
    | ExitPatch
    | AddNode NodeId
    | UpdateNode NodeId (NodeMsg n c a x)
    | ForgetNode NodeId
    | Connect NodeId NodeId OutletId InletId
    | Disconnect NodeId NodeId OutletId InletId


data NodeMsg n c a x
    = InitNode n String
    | AddInlet InletId
    | UpdateInlet InletId (InletMsg c a x)
    | ForgetInlet InletId
    | AddOutlet OutletId
    | UpdateOutlet OutletId (OutletMsg c)
    | ForgetOutlet OutletId


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
    NetworkT
        { messages :: S.Signal (NetworkMsg n c a x)
        , patches :: Map PatchId (Patch n c a x)
        , nodes :: Map NodeId (Node n c a x)
        , inlets :: Map InletId (Inlet c a x)
        , outlets :: Map OutletId (Outlet c a x)
        , links ::  Map LinkId (Link a x)
        , selected :: Maybe PatchId
        , entered :: Array PatchId
        }


data Patch n c a x =
    PatchT
        { id :: PatchId
        , title :: String
        , nodes :: Array NodeId
        , links :: Array LinkId
        }


data Node n c a x =
    NodeT
        { id :: NodeId
        , title :: String
        , type :: Maybe n
        , inlets :: Array InletId
        , outlets :: Array OutletId
        , process :: Maybe (Map InletId (Value a x) -> Map OutletId (Value a x))
        }
        (ProcessSignal a x)


data Inlet c a x =
    InletT
        { id :: InletId
        , label :: String
        , type :: Maybe c
        }
        (FlowSignal a x)


data Outlet c a x =
    OutletT
        { id :: OutletId
        , label :: String
        , type :: Maybe c
        }
        (FlowSignal a x)


data Link a x = LinkT
        { id :: LinkId
        , inlet :: InletId
        , outlet :: OutletId
        }
        (FlowSignal a x)


newtype Actions' e a =
    Actions' (Eff (channel :: SC.CHANNEL | e) (SC.Channel a))

newtype TaggedActions' e a i  =
    TaggedActions'
        (Eff (channel :: SC.CHANNEL | e) (Tuple i (SC.Channel a)))


-- newtype TaggedActions' e  =
--     TaggedActions'
--         (Eff (channel :: SC.CHANNEL | e) (Tuple String (SC.Channel Int)))


type NetworkActions' e n c a x = Actions' e (NetworkMsg n c a x)
type PatchActions' e n c a x = TaggedActions' e (PatchMsg n c a x) PatchId
type NodeActions' e n c a x = TaggedActions' e (NodeMsg n c a x) NodeId
type InletActions' e c a x = TaggedActions' e (InletMsg c a x) InletId
type OutletActions' e c = TaggedActions' e (OutletMsg c) OutletId


data Actions e n c a x
    = NetworkActions (NetworkActions' e n c a x)
    | PatchActions (PatchActions' e n c a x)
    | NodeActions (NodeActions' e n c a x)
    | InletActions (InletActions' e c a x)
    | OutletActions (OutletActions' e c)
    -- | LinkActions (LinkActions' c a x)


-- API:


network' :: forall n c a x. Network n c a x
network' =
    NetworkT
        { messages : S.constant Start
        , patches : Map.empty
        , nodes : Map.empty
        , inlets : Map.empty
        , outlets : Map.empty
        , links :  Map.empty
        , selected : Nothing
        , entered : []
        }


patch' :: forall n c a x. Patch n c a x
patch' =
    PatchT
        { id : "test"
        , title : "Noname"
        , nodes : []
        , links : []
        }


node' :: forall n c a x. Node n c a x
node' =
    NodeT
        { id : "test"
        , title : "Unknown"
        , type : Nothing
        , inlets : []
        , outlets : []
        , process : Nothing
        }
        (S.constant (Tuple Map.empty Map.empty)) -- FIXME: make simple message


inlet' :: forall c a x. Inlet c a x
inlet' =
    InletT
        { id: "test"
        , label : "foo"
        , type : Nothing
        }
        (S.constant Bang)


outlet' :: forall c a x. Outlet c a x
outlet' =
    OutletT
        { id: "test"
        , label : "foo"
        , type : Nothing
        }
        (S.constant Bang)



getId :: forall e a i. TaggedActions' e a i -> Eff ( channel :: SC.CHANNEL | e) i
getId (TaggedActions' eff) = do
    (Tuple id _) <- liftEff eff
    pure $ id


actions :: forall e a i. a -> Actions' e a
actions default = Actions' $ do
    chan <- SC.channel default
    pure $ chan


taggedActions :: forall e a i. i -> a -> TaggedActions' e a i
taggedActions id default = TaggedActions' $ do
    chan <- SC.channel default
    pure $ Tuple id chan


sendMsg :: forall e a i. a -> TaggedActions' e a i -> TaggedActions' e a i
sendMsg msg (TaggedActions' eff) = TaggedActions' $ do
    (Tuple id chan) <- liftEff eff
    SC.send chan msg
    pure $ Tuple id chan


sendMsg' :: forall e a. a -> Actions' e a -> Actions' e a
sendMsg' msg (Actions' eff) = Actions' $ do
    chan <- liftEff eff
    SC.send chan msg
    pure $ chan


sendMsgUp :: forall e a b i. (i -> a) -> TaggedActions' e b i -> Actions' e a -> Actions' e a
sendMsgUp msgF (TaggedActions' teff) (Actions' eff) = Actions' $ do
    (Tuple id _) <- liftEff teff
    chan <- liftEff eff
    SC.send chan $ msgF id
    pure $ chan


sendMsgUpAndSubscribe
    :: forall e a b i
     . (i -> a)
    -> (b -> i -> a)
    -> TaggedActions' e b i
    -> Actions' e a
    -> Actions' e a
sendMsgUpAndSubscribe msgF toUpperF (TaggedActions' teff) (Actions' eff) = Actions' $ do
    (Tuple srcId srcChan) <- liftEff teff
    dstChan <- liftEff eff
    SC.send dstChan $ msgF srcId
    _ <- S.unwrap $ SC.subscribe srcChan S.~>
        (\srcMsg -> SC.send dstChan $ toUpperF srcMsg srcId)
    pure $ dstChan


sendMsgUpAndSubscribe'
    :: forall e a b ia ib
     . (ib -> a)
    -> (b -> ib -> a)
    -> TaggedActions' e b ib
    -> TaggedActions' e a ia
    -> TaggedActions' e a ia
sendMsgUpAndSubscribe' msgF toUpperF (TaggedActions' srcEff) (TaggedActions' dstEff) =
    TaggedActions' $ do
        (Tuple srcId srcChan) <- liftEff srcEff
        (Tuple dstId dstChan) <- liftEff dstEff
        SC.send dstChan $ msgF srcId
        _ <- S.unwrap $ SC.subscribe srcChan S.~>
            (\srcMsg -> SC.send dstChan $ toUpperF srcMsg srcId)
        pure $ Tuple dstId dstChan


network :: forall e n c a x. NetworkActions' e n c a x
network = actions Start


patch :: forall e n c a x. String -> PatchActions' e n c a x
patch title = taggedActions title (InitPatch title)


node :: forall e n c a x. n -> String -> NodeActions' e n c a x
node type_ title = taggedActions title (InitNode type_ title)


inlet :: forall e c a x. c -> String -> InletActions' e c a x
inlet type_ label = taggedActions label (InitInlet type_ label)


outlet :: forall e c. c -> String -> OutletActions' e c
outlet type_ label = taggedActions label (InitOutlet type_ label)


addPatch
    :: forall e n c a x
     . PatchActions' e n c a x
    -> NetworkActions' e n c a x
    -> NetworkActions' e n c a x
addPatch patchActions networkActions =
    sendMsgUpAndSubscribe
        (\patchId -> AddPatch patchId)
        (\patchMsg patchId -> UpdatePatch patchId patchMsg)
        patchActions
        networkActions


removePatch
    :: forall e n c a x
     . PatchActions' e n c a x
    -> NetworkActions' e n c a x
    -> NetworkActions' e n c a x
removePatch patchActions networkActions =
    sendMsgUp
        (\patchId -> ForgetPatch patchId)
        patchActions
        networkActions


select :: forall e n c a x. PatchActions' e n c a x -> PatchActions' e n c a x
select patchActions = sendMsg SelectPatch patchActions


deselect :: forall e n c a x. PatchActions' e n c a x -> PatchActions' e n c a x
deselect patchActions = sendMsg DeselectPatch patchActions


enter :: forall e n c a x. PatchActions' e n c a x -> PatchActions' e n c a x
enter patchActions = sendMsg EnterPatch patchActions


exit :: forall e n c a x. PatchActions' e n c a x -> PatchActions' e n c a x
exit patchActions = sendMsg ExitPatch patchActions


addNode
    :: forall e n c a x
     . NodeActions' e n c a x
    -> PatchActions' e n c a x
    -> PatchActions' e n c a x
addNode nodeActions patchActions =
    -- patchActions -- FIXME: implement
    sendMsgUpAndSubscribe'
        (\nodeId -> AddNode nodeId)
        (\nodeMsg nodeId -> UpdateNode nodeId nodeMsg)
        nodeActions
        patchActions


removeNode
    :: forall e n c a x
     . NodeActions' e n c a x
    -> PatchActions' e n c a x
    -> PatchActions' e n c a x
removeNode nodeActions patchActions =
    patchActions -- FIXME: implement
    -- tellAndPerform' (ForgetInlet inlet.id) updateNode node


addInlet
    :: forall e n c a x
     . InletActions' e c a x
    -> NodeActions' e n c a x
    -> NodeActions' e n c a x
addInlet inletActions nodeActions =
    nodeActions -- FIXME: implement
    -- tellAndPerform (UpdateInlet inletActions inlet) updateNode nodeActions


removeInlet
    :: forall e n c a x
     . InletActions' e c a x
    -> NodeActions' e n c a x
    -> NodeActions' e n c a x
removeInlet inletActions nodeActions =
    nodeActions -- FIXME: implement
    -- tellAndPerform (ForgetInlet inlet.id) updateNode nodeActions


addOutlet
    :: forall e n c a x
     . OutletActions' e c
    -> NodeActions' e n c a x
    -> NodeActions' e n c a x
addOutlet outletActions nodeActions =
    nodeActions -- FIXME: implement
    -- tellAndPerform' (UpdateOutlet outletActions outlet) updateNode nodeActions


removeOutlet
    :: forall e n c a x
     . OutletActions' e c
    -> NodeActions' e n c a x
    -> NodeActions' e n c a x
removeOutlet outletAction nodeActions =
    nodeActions -- FIXME: implemeent


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
update Start network = network
update (UpdatePatch patchId patchMsg) (NetworkT network'@{ patches, nodes }) =
    let
        patches' = patches
                -- use Map.alter instead?
                |> Map.update (\patch -> Just $ updatePatch patchMsg patch) patchId
        nodes' = case patchMsg of
            UpdateNode nodeId nodeMsg ->
                nodes |> Map.update (\node -> Just $ updateNode nodeMsg node) nodeId
            ForgetNode nodeId ->
                nodes |> Map.delete nodeId
            _ -> nodes
    in
        NetworkT network'
            { patches = patches'
            , nodes = nodes'
            }
update (ForgetPatch patchId) (NetworkT network'@{ patches }) =
    NetworkT network'
        { patches =
            patches |> Map.delete patchId }
update _ network = network -- FIXME : implement


updatePatch :: forall n c a x. PatchMsg n c a x -> Patch n c a x -> Patch n c a x
updatePatch (InitPatch title) patch = patch
updatePatch (Connect srcId dstId outletId inletId) patch = patch -- TODO: implement
updatePatch _ patch = patch -- FIXME : implement


updateNode :: forall n c a x. NodeMsg n c a x -> Node n c a x -> Node n c a x
updateNode (InitNode type_ title) node = node
updateNode _ node = node -- FIXME : implement


updateInlet :: forall c a x. InletMsg c a x -> Inlet c a x -> Inlet c a x
updateInlet (InitInlet type_ label) inlet = inlet
updateInlet _ inlet = inlet -- FIXME : implement


updateOutlet :: forall c a x. OutletMsg c -> Outlet c a x -> Outlet c a x
updateOutlet (InitOutlet type_ label) outlet = outlet
updateOutlet _ outlet = outlet -- FIXME : implement


tagFlowSignal :: forall n c a x. Inlet c a x -> TaggedFlowSignal a x
tagFlowSignal (InletT inlet' flowSignal) =
    flowSignal S.~> (\val -> Tuple inlet'.id val)


-- make data items require a Show instance,
-- maybe even everywhere. Also create some type class which defines interfaces
-- for Node type and Channel type?
-- like accept() allow() etc.


instance showNetwork :: Show (Network n c a x) where
    show (NetworkT network) = "(Network \n"
        <> show (Map.size network.patches) <> " Patches\n"
        <> show (Map.size network.nodes) <> " Nodes\n"
        <> show (Map.size network.inlets) <> " Inlets\n"
        <> show (Map.size network.outlets) <> " Outlets\n"
        <> "Selected Patch: " <> show network.selected
        <> "Entered Patches: " <> show network.entered
        <> ")"


instance showPatch :: Show (Patch n c a x) where
    show (PatchT patch) = "(Patch " <> patch.id <> "\n"
        <> show (Array.length patch.nodes) <> " Nodes\n"
        <> show (Array.length patch.links) <> " Links\n"
        <> ")"


instance showNode :: Show n => Show (Node n c a x) where
    show (NodeT node _) = "(Node " <> node.id <> "\n"
        <> show node.type <> " "
        <> show (Array.length node.inlets) <> " Inlets\n"
        <> show (Array.length node.outlets) <> " Outlets\n"
        <> ")"


instance showInlet :: Show c => Show (Inlet c a x) where
    show (InletT inlet _) =
        "(Inlet " <> inlet.id <> " "
                  <> show inlet.type <> " "
                  <> inlet.label <> ")"


instance showOutlet :: Show c => Show (Outlet c a x) where
    show (OutletT outlet _) =
        "(Outlet " <> show outlet.type <> " "
                   <> outlet.id <> " "
                   <> outlet.label <> ")"


instance showNetworkMsg :: ( Show n, Show c ) => Show (NetworkMsg n c a x) where
    show Start = "Start"
    show (AddPatch patchId) = "Add patch: " <> patchId
    show (UpdatePatch patchId patchMsg) = "Update patch: " <> patchId <> " -> " <> show patchMsg
    show (ForgetPatch patchId) = "Forget patch: " <> patchId
    show Stop = "Stop"


instance showPatchMsg :: ( Show n, Show c ) => Show (PatchMsg n c a x) where
    show (InitPatch title) = "Init patch: " <> title
    show SelectPatch = "Select patch"
    show DeselectPatch = "Deselect patch"
    show EnterPatch = "Enter patch"
    show ExitPatch = "Exit patch"
    show (AddNode nodeId) = "Add node: " <> nodeId
    show (UpdateNode nodeId nodeMsg) = "Update node: " <> nodeId <> " -> " <> show nodeMsg
    show (ForgetNode nodeId) = "Remove node:"  <> nodeId
    show (Connect srcNodeId dstNodeId outletId inletId) = "Connect:\n"
        <> "Node " <> srcNodeId <> " -> Node " <> dstNodeId <> "\n"
        <> "Outlet " <> outletId <> " -> Inlet " <> inletId
    show (Disconnect srcNodeId dstNodeId outletId inletId) = "Disconnect:\n"
        <> "Node " <> srcNodeId <> " -> Node " <> dstNodeId <> "\n"
        <> "Outlet " <> outletId <> " -> Inlet " <> inletId


instance showNodeMsg :: ( Show n, Show c ) => Show (NodeMsg n c a x) where
    show (InitNode type_ title) = "Init node: " <> show type_ <> " " <> title
    show (AddInlet inletId) = "Add inlet: " <> inletId
    show (UpdateInlet inletId inletMsg) = "Update inlet: " <> inletId <> " -> " <> show inletMsg
    show (ForgetInlet inletId) = "Forhet inlet:"  <> inletId
    show (AddOutlet outletId) = "Add outlet: " <> outletId
    show (UpdateOutlet outletId outletMsg) = "Update outlet: " <> outletId <> " -> " <> show outletMsg
    show (ForgetOutlet outletId) = "Remove outlet:"  <> outletId


instance showInletMsg :: Show c => Show (InletMsg c a x) where
    show (InitInlet type_ label) = "Init inlet: " <> show type_ <> " " <> label
    show (ConnectToOutlet outletId _) = "Connect to outlet: " <> outletId
    show (DisconnectFromOutlet outletId) = "Disconnect from outlet:"  <> outletId
    show HideInlet = "Hide inlet"
    show RevealInlet = "Reveal inlet"


instance showOutletMsg :: Show c => Show (OutletMsg c) where
    show (InitOutlet type_ label) = "Init outlet: " <> show type_ <> " " <> label
    show (ConnectToInlet inletId) = "Connect to inlet: " <> inletId
    show (DisconnectFromInlet inletId) = "Disconnect from inlet:"  <> inletId


instance showValue :: ( Show a, Show x ) => Show (Value a x) where
    show Bang = "Bang"
    show (Data val) = "Data: " <> show val
    show (Error err) = "Error: " <> show err
    show (SysError msg) = "System Error: " <> msg


log
    :: forall n c a x
     . Show n => Show c => Show a => Show x
    => Network n c a x -> S.Signal String
log = logNetwork


logNetwork
    :: forall n c a x
     . Show n => Show c => Show a => Show x
    => Network n c a x -> S.Signal String
logNetwork (NetworkT network') =
    network'.messages S.~> show


-- logDataFlow :: forall n c a x. Show a => Show x => Network n c a x -> S.Signal String
-- logDataFlow (NetworkT network') =
--     let
--         allNodes = List.foldr
--             (\(PatchT patch') allNodes ->
--                 allNodes <> values patch'.nodes) empty (values network'.patches)
--         allDataSignals =
--             List.foldr (\(NodeT node' dataSignal) allSignals ->
--                 allSignals
--                     <> (map (\(InletT inlet' dataSignal) -> dataSignal) (values node'.inlets))
--                     <> (map (\(OutletT outlet' dataSignal) -> dataSignal) (values node'.outlets))
--             ) empty allNodes
--     in
--         case S.mergeMany allDataSignals of
--             Just signal -> map show signal
--             Nothing -> S.constant "Empty"
