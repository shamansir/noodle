module Rpd
    ( Id, PatchId, NodeId, ChannelId, InletId, OutletId, LinkId
    , App(..), Renderer
    , Network, Patch, Node, Inlet, Outlet, Link
    -- TRY TO REMOVE LATER
    , NetworkMsg(..), PatchMsg(..), NodeMsg(..), InletMsg(..), OutletMsg(..)
    , FlowSignal, Value(..), Actions
    -- END OF TRY TO REMOVE LATER
    , run, getMessages
    , network, patch, node, inlet, outlet
    , addPatch, removePatch, select, deselect, enter, exit
    , addNode
    , addInlet, getInlet, addOutlet, getOutlet, process, connect
    , allow, default, send
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
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
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
    | RequestPatch PatchId
    | UpdatePatch PatchId (PatchMsg n c a x)
    | ForgetPatch PatchId
    | Stop


data PatchMsg n c a x
    = RequestPatchAccess
    | InitPatch String
    | SelectPatch
    | DeselectPatch
    | EnterPatch
    | ExitPatch
    | RequestNode NodeId -- create or update
    | UpdateNode NodeId (NodeMsg n c a x)
    | ForgetNode NodeId
    | Connect NodeId NodeId OutletId InletId
    | Disconnect NodeId NodeId OutletId InletId


data NodeMsg n c a x
    = RequestNodeAccess
    | InitNode n String
    | RequestInlet InletId
    | UpdateInlet InletId (InletMsg c a x)
    | ForgetInlet InletId
    | GetOutlet OutletId
    | RequestOutlet OutletId -- create or update
    | UpdateOutlet OutletId (OutletMsg c)
    | ForgetOutlet OutletId


data InletMsg c a x
    = RequestInletAccess
    | InitInlet c String
    | Allow (Map c (Unit -> a))
    | ConnectToOutlet OutletId (FlowSignal a x)
    | DisconnectFromOutlet OutletId
    | HideInlet
    | RevealInlet


data OutletMsg c
    = InitOutlet c String
    | RequestOutletAccess
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

-- type TaggedFlowChannel a x = SC.Channel (InletId /\ (Value a x))
type TaggedFlowSignal a x = S.Signal (InletId /\ (Value a x))

-- The signal where the messages go
--type MsgChannel m = SC.Channel m
-- type MsgEmitter eff m = SL.Emitter eff m

-- The special channel for nodes which tracks the data flow through node inputs and outlets
-- type ProcessChannel a x = SC.Channel (Map InletId (Value a x) /\ Map OutletId (Value a x))
type ProcessSignal a x = S.Signal (Map InletId (Value a x) /\ Map OutletId (Value a x))


type ProcessF a x = (Map InletId (Value a x) -> Map OutletId (Value a x))


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
        , process :: Maybe (ProcessF a x)
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


newtype Actions e msg id = Actions (Eff (channel :: SC.CHANNEL | e) (id /\ (SC.Channel msg)))


-- make a general type : Actions, and use Tuple to
-- send pairs of ID and Message to the signal?


type NetworkActions e n c a x = Actions e (NetworkMsg n c a x) Unit
type PatchActions e n c a x = Actions e (PatchMsg n c a x) PatchId
type NodeActions e n c a x = Actions e (NodeMsg n c a x) NodeId
type InletActions e c a x = Actions e (InletMsg c a x) InletId
type OutletActions e c = Actions e (OutletMsg c) OutletId


-- or just use a data type below, just restrict methods to special types
-- by removing the quote and the types with quote?

-- remove `c`` since it can be replaced with `a`, or vice versa

-- if messages will fail to record, pass the function to
-- `Rpd.run` as the last argument and call it with created
-- channel after subscribing to it. then, require this channel
-- to be passed to `Rpd.network` so it will use it as a target
-- for messages and subscribe all children to it?


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
        (S.constant (Map.empty /\ Map.empty)) -- FIXME: make simple message


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



getId :: forall e a i. Actions e a i -> Eff ( channel :: SC.CHANNEL | e) i
getId eff = do
    (id /\ _) <- liftEff eff
    pure $ id


-- Init `Actions` channel with given message
actions :: forall e a i. a -> Actions e a i
actions default = taggedActions unit default


-- Init `Actions` channel with given ID and message
taggedActions :: forall e a i. i -> a -> Actions e a i
taggedActions id default = Actions $ do
    chan <- SC.channel default
    pure $ id /\ chan


-- Send given message to the `Actions` channel
-- sendMsg :: forall e a. a -> Actions' e a -> Actions' e a
-- sendMsg msg eff = do
--     chan <- liftEff eff
--     SC.send chan msg
--     pure $ chan


-- Send given message to the `Actions` channel
sendMsg :: forall e a i. a -> Actions e a i -> Actions e a i
sendMsg msg (Actions eff) = Actions $ do
    (id /\ chan) <- liftEff eff
    SC.send chan msg
    pure $ id /\ chan


-- Given the child `Actions` channel, adapt message with `msgF`
-- and send it to the parent `Actions` channel
sendMsgUp :: forall e a b i j. (j -> a) -> Actions e b j -> Actions e a i -> Actions e a i
sendMsgUp msgF (Actions srcEff) (Actions trgEff) = Actions $ do
    (srcId /\ _) <- liftEff srcEff
    (trgId /\ chan) <- liftEff trgEff
    SC.send chan $ msgF srcId
    pure $ trgId /\ chan


-- Given the child `Actions` channel, adapt message with `msgF`
-- and send it to the parent `Actions` channel,
-- then subscribe parent channel to all the messages sent from a child,
-- using `toUpperF` to adapt them to parent scope.
sendMsgUpAndSubscribe
    :: forall e a b i j
     . (j -> a)
    -> (b -> j -> a)
    -> Actions e b j
    -> Actions e a i
    -> Actions e a i
sendMsgUpAndSubscribe msgF toUpperF (Actions srcEff) (Actions dstEff) = Actions $ do
-- sendMsgUpAndSubscribe msgF toUpperF teff eff = do
    (srcId /\ srcChan) <- liftEff srcEff
    (dstId /\ dstChan) <- liftEff dstEff
    SC.send dstChan $ msgF srcId
    _ <- S.unwrap $ SC.subscribe srcChan S.~>
        (\srcMsg -> SC.send dstChan $ toUpperF srcMsg srcId)
    pure $ dstId /\ dstChan
    -- (srcId /\ srcChan) <- liftEff teff
    -- dstChan <- liftEff eff
    -- SC.send dstChan $ msgF srcId
    -- _ <- S.unwrap $ SC.subscribe srcChan S.~>
    --     (\srcMsg -> SC.send dstChan $ toUpperF srcMsg srcId)
    -- pure $ dstChan


-- Given an ID and default message, create new child `Actions` channel,
-- send default message there, then adapt next message with `msgF`
-- and send it to the parent `Actions` channel,
-- then subscribe parent channel to all the messages sent from a child,
-- using `toUpperF` to adapt them to parent scope.
-- Return the created child channel.
requestAccess
    :: forall e a b i j
     . j
    -> b
    -> (j -> a)
    -> (b -> j -> a)
    -> Actions e a i
    -> Actions e b j
requestAccess srcId defMsg msgF toUpperF (Actions dstEff) = Actions $ do
    srcChan <- SC.channel defMsg
    (dstId /\ dstChan) <- liftEff dstEff
    SC.send dstChan $ msgF srcId
    _ <- S.unwrap $ SC.subscribe srcChan S.~>
        (\srcMsg -> SC.send dstChan $ toUpperF srcMsg srcId)
    pure $ srcId /\ srcChan


getMessages :: forall n c a x e. App n c a x e -> S.Signal (NetworkMsg n c a x)
getMessages (App { network }) =
    case network of
        Just (NetworkT network') -> network'.messages
        Nothing -> S.constant (Start)


network :: forall e n c a x. NetworkActions e n c a x
network = (actions Start)


patch :: forall e n c a x. String -> PatchActions e n c a x
patch title = taggedActions title (InitPatch title)


node :: forall e n c a x. n -> String -> NodeActions e n c a x
node type_ title = taggedActions title (InitNode type_ title)


inlet :: forall e n c a x. c -> String -> InletActions e n c a x
inlet type_ label = taggedActions label (InitInlet type_ label)


outlet :: forall e n c a x. c -> String -> OutletActions e n c a x
outlet type_ label = taggedActions label (InitOutlet type_ label)


addPatch
    :: forall e n c a x
     . PatchActions e n c a x
    -> NetworkActions e n c a x
    -> NetworkActions e n c a x
addPatch patchActions networkActions =
    sendMsgUpAndSubscribe
        RequestPatch
        (flip UpdatePatch)
        patchActions
        networkActions


removePatch
    :: forall e n c a x
     . PatchActions e n c a x
    -> NetworkActions e n c a x
    -> NetworkActions e n c a x
removePatch patchActions networkActions =
    sendMsgUp
        ForgetPatch
        patchActions
        networkActions


select :: forall e n c a x. PatchActions e n c a x -> PatchActions e n c a x
select patchActions = sendMsg SelectPatch patchActions


deselect :: forall e n c a x. PatchActions e n c a x -> PatchActions e n c a x
deselect patchActions = sendMsg DeselectPatch patchActions


enter :: forall e n c a x. PatchActions e n c a x -> PatchActions e n c a x
enter patchActions = sendMsg EnterPatch patchActions


exit :: forall e n c a x. PatchActions e n c a x -> PatchActions e n c a x
exit patchActions = sendMsg ExitPatch patchActions


addNode
    :: forall e n c a x
     . NodeActions e n c a x
    -> PatchActions e n c a x
    -> PatchActions e n c a x
addNode nodeActions patchActions =
    sendMsgUpAndSubscribe
        RequestNode
        (flip UpdateNode)
        nodeActions
        patchActions


removeNode
    :: forall e n c a x
     . NodeActions e n c a x
    -> PatchActions e n c a x
    -> PatchActions e n c a x
removeNode nodeActions patchActions =
    patchActions -- FIXME: implement


addInlet
    :: forall e n c a x
     . InletActions e c a x
    -> NodeActions e n c a x
    -> NodeActions e n c a x
addInlet inletActions nodeActions =
    nodeActions -- FIXME: implement



connect
    :: forall e n c a x
     . InletActions e c a x
    -> OutletActions e c
    -> OutletActions e c
    -- -> NodeActions e n c a x
connect inletActions outletActions =
    outletActions -- FIXME: implement


getInlet
    :: forall e n c a x
     . InletId
    -> NodeActions e n c a x
    -> InletActions e c a x
getInlet inletId nodeActions =
    requestAccess
        inletId
        RequestInletAccess
        RequestInlet
        (flip UpdateInlet)
        nodeActions


getOutlet
    :: forall e n c a x
     . OutletId
    -> NodeActions e n c a x
    -> OutletActions e c
getOutlet outletId nodeActions =
    requestAccess
        outletId
        RequestOutletAccess
        RequestOutlet
        (flip UpdateOutlet)
        nodeActions


removeInlet
    :: forall e n c a x
     . InletActions e c a x
    -> NodeActions e n c a x
    -> NodeActions e n c a x
removeInlet inletActions nodeActions =
    nodeActions -- FIXME: implement


addOutlet
    :: forall e n c a x
     . OutletActions e c
    -> NodeActions e n c a x
    -> NodeActions e n c a x
addOutlet outletActions nodeActions =
    nodeActions -- FIXME: implement


removeOutlet
    :: forall e n c a x
     . OutletActions e c
    -> NodeActions e n c a x
    -> NodeActions e n c a x
removeOutlet outletAction nodeActions =
    nodeActions -- FIXME: implemeent


process
    :: forall e n c a x
     . ProcessF a x
    -> NodeActions e n c a x
    -> NodeActions e n c a x
process processF nodeActions =
    nodeActions -- FIXME: implemeent


allow
    :: forall e n c a x
     . Array (c /\ (Unit -> a))
    -> InletActions e c a x
    -> InletActions e c a x
allow list inletActions =
    inletActions -- FIXME: implement


default
    :: forall e n c a x
     . a
    -> InletActions e c a x
    -> InletActions e c a x
default val inletActions =
    inletActions -- FIXME: implement


send
    :: forall e n c a x
     . a
    -> InletActions e c a x
    -> InletActions e c a x
send val inletActions =
    inletActions -- FIXME: implement


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
     . Array (Renderer n c a x eff)
    -> Actions eff n c a x
    -> Eff (channel :: SC.CHANNEL | eff) (App n c a x eff)
run renderers networkActions = do
    c <- SC.channel Start
    let s = SC.subscribe c
        network = network'
        app =
            App
                { network :Just network
                , renderers : renderers
                }
    -- SC.send c Stop
    -- SC.send c (Array.head messages |> fromMaybe Stop)
    -- S.runSignal (f s)
    -- Array.foldM
    --     (\msgStack msg -> do
    --         SC.send c msg
    --         --fromMaybe [] (tail msgStack)
    --     )
    --     Unit.unit
    --     (Array.tail messages |> fromMaybe [ Stop ])
    pure app


initProcessChannel :: forall a x. ProcessSignal a x
initProcessChannel =
    S.constant (Map.empty /\ Map.empty)


update :: forall n c a x. NetworkMsg n c a x -> Network n c a x -> Network n c a x
update Start network = network
update (UpdatePatch patchId patchMsg)
       (NetworkT network'@{ patches, nodes, inlets, outlets }) =
    let
        patches' = patches
                -- use Map.alter instead?
                |> Map.update (\patch -> Just $ updatePatch patchMsg patch) patchId
        nodes' = case patchMsg of
            UpdateNode nodeId nodeMsg ->
                nodes |> Map.update (\node -> Just $ updateNode nodeMsg node) nodeId
            ForgetNode nodeId ->
                nodes |> Map.delete nodeId
            -- TODO: connect / disconnect
            _ -> nodes
        inlets' = case patchMsg of
            (UpdateNode _ (UpdateInlet inletId inletMsg)) ->
                inlets |> Map.update (\inlet -> Just $ updateInlet inletMsg inlet) inletId
            (UpdateNode _ (ForgetInlet inletId)) ->
                inlets |> Map.delete inletId
            _ -> inlets
        outlets' = case patchMsg of
            (UpdateNode _ (UpdateOutlet outletId outletMsg)) ->
                outlets |> Map.update (\outlet -> Just $ updateOutlet outletMsg outlet) outletId
            (UpdateNode _ (ForgetOutlet outletId)) ->
                outlets |> Map.delete outletId
            _ -> outlets
    in
        NetworkT network'
            { patches = patches'
            , nodes = nodes'
            , inlets = inlets'
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
    flowSignal S.~> (\val -> inlet'.id /\ val)


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
    show (RequestPatch patchId) = "Request patch: " <> patchId
    show (UpdatePatch patchId patchMsg) = "Update patch: " <> patchId <> " -> " <> show patchMsg
    show (ForgetPatch patchId) = "Forget patch: " <> patchId
    show Stop = "Stop"


instance showPatchMsg :: ( Show n, Show c ) => Show (PatchMsg n c a x) where
    show (InitPatch title) = "Init patch: " <> title
    show RequestPatchAccess = "Request patch access"
    show SelectPatch = "Select patch"
    show DeselectPatch = "Deselect patch"
    show EnterPatch = "Enter patch"
    show ExitPatch = "Exit patch"
    show (RequestNode nodeId) = "Request node: " <> nodeId
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
    show RequestNodeAccess = "Request node access"
    show (RequestInlet inletId) = "Request inlet: " <> inletId
    show (UpdateInlet inletId inletMsg) = "Update inlet: " <> inletId <> " -> " <> show inletMsg
    show (ForgetInlet inletId) = "Forhet inlet:"  <> inletId
    show (RequestOutlet outletId) = "Request outlet: " <> outletId
    show (GetOutlet outletId) = "Get inlet: " <> outletId
    show (UpdateOutlet outletId outletMsg) = "Update outlet: " <> outletId <> " -> " <> show outletMsg
    show (ForgetOutlet outletId) = "Remove outlet:"  <> outletId


instance showInletMsg :: Show c => Show (InletMsg c a x) where
    show (InitInlet type_ label) = "Init inlet: " <> show type_ <> " " <> label
    show RequestInletAccess = "Request inlet access"
    show (ConnectToOutlet outletId _) = "Connect to outlet: " <> outletId
    show (DisconnectFromOutlet outletId) = "Disconnect from outlet:"  <> outletId
    show (Allow allowMap) = "Allow: " <>
        (Map.keys allowMap |> map show |> Array.fromFoldable |> String.joinWith ", ")
    show HideInlet = "Hide inlet"
    show RevealInlet = "Reveal inlet"


instance showOutletMsg :: Show c => Show (OutletMsg c) where
    show (InitOutlet type_ label) = "Init outlet: " <> show type_ <> " " <> label
    show RequestOutletAccess = "Request outlet access"
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
