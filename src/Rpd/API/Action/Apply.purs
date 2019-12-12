module Rpd.API.Action.Apply where

import Prelude
import Effect (Effect)

import Data.Maybe
import Data.String (take) as String
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Sequence (empty, toUnfoldable) as Seq
import Data.Lens (view, setJust, set)
import Data.Array ((:))
import Data.Foldable (class Foldable, foldr)
import Data.Traversable (traverse, traverse_)

import Debug.Trace as DT

import FRP.Event as E
import FRP.Event.Class (count) as E
import FRP.Event.Time as E

import Rpd.Util (PushableFlow(..), Canceler)
import Rpd.API as Api
import Rpd.API (uuidByPath, makePushableFlow)
import Rpd.API.Errors (RpdError)
import Rpd.API.Errors as Err
import Rpd.API.Action
    ( Action(..)
    , InnerAction(..)
    , RequestAction(..)
    , BuildAction(..)
    , DataAction(..)
    , RpdEffect(..)
    )
import Rpd.Network
import Rpd.Process
import Rpd.Optics
import Rpd.Path as Path
import Rpd.Toolkit
import Rpd.UUID as UUID


type Step d c n = Either RpdError (Network d c n /\ Array (RpdEffect d c n))


infixl 1 next as <∞>


-- TODO: make an operator?
-- TODO: close to Actions sequensing?
next :: forall d c n. Step d c n -> (Network d c n -> Step d c n) -> Step d c n
next stepA stepB = do  -- FIXME: `WriterT`?
    (nw /\ effs) <- stepA
    (nw' /\ effs') <- stepB nw
    pure $ nw' /\ (effs <> effs')


foldSteps
    :: forall d c n x f
     . Foldable f
    => Network d c n
    -> f x
    -> (x -> Network d c n -> Step d c n)
    -> Step d c n
foldSteps initNW foldable foldF =
    foldr
        (\x step -> step <∞> foldF x)
        (pure $ initNW /\ [])
        foldable


apply
    :: forall d c n
     . Toolkit d c n
    -> Action d c n
    -> Network d c n
    -> Step d c n
apply _ NoOp nw =
    pure $ nw /\ []
apply toolkit (Inner innerAction) nw = applyInnerAction toolkit innerAction nw
apply toolkit (Request requestAction) nw = applyRequestAction toolkit requestAction nw
apply toolkit (Build buildAction) nw = applyBuildAction toolkit buildAction nw
apply toolkit (Data dataAction) nw = applyDataAction toolkit dataAction nw


applyDataAction
    :: forall d c n
     . Toolkit d c n
    -> DataAction d c
    -> Network d c n
    -> Step d c n
applyDataAction _ Bang nw =
    pure $ nw /\ []
applyDataAction _ (GotInletData _ _) nw =
    pure $ nw /\ []
applyDataAction _ (GotOutletData _ _) nw =
    pure $ nw /\ []
applyDataAction _ (SendToInlet _ _) nw = -- FIXME: either implement or get rid of
    pure $ nw /\ []
applyDataAction _ (SendToOutlet _ _) nw = -- FIXME: either implement or get rid of
    pure $ nw /\ []


applyRequestAction
    :: forall d c n
     . Toolkit d c n
    -> RequestAction d c n
    -> Network d c n
    -> Step d c n
applyRequestAction _ (ToAddPatch alias) nw =
    pure $ nw /\ [ AddPatchE alias ]
applyRequestAction (Toolkit _ getDef) (ToAddNode patchPath alias n) nw =
    pure $ nw /\ [ AddNodeE patchPath alias n $ getDef n ]
applyRequestAction (Toolkit _ getDef) (ToAddNextNode patchPath n) nw = do
    pure $ nw /\ [ AddNextNodeE patchPath n $ getDef n ]
applyRequestAction _ (ToAddNodeByDef patchPath alias n def) nw =
    pure $ nw /\ [ AddNodeE patchPath alias n def ]
applyRequestAction _ (ToAddNextNodeByDef patchPath n def) nw = do
    pure $ nw /\ [ AddNextNodeE patchPath n def ]
    -- | AddNodeE Path.ToPatch Path.Alias n (NodeDef d c)
applyRequestAction tk (ToRemoveNode nodePath) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    applyBuildAction tk (RemoveNode node) nw
applyRequestAction _ (ToAddInlet nodePath alias c) nw =
    pure $ nw /\ [ AddInletE nodePath alias c ]
applyRequestAction _ (ToAddOutlet nodePath alias c) nw =
    pure $ nw /\ [ AddOutletE nodePath alias c ]
applyRequestAction tk (ToRemoveInlet inletPath) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    applyBuildAction tk (RemoveInlet inlet) nw
applyRequestAction tk (ToRemoveOutlet outletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    applyBuildAction tk (RemoveOutlet outlet) nw
applyRequestAction _ (ToProcessWith nodePath processF) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    pure $ nw /\ [ ProcessWithE node processF ]
applyRequestAction _ (ToConnect outletPath inletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    pure $ nw /\ [ AddLinkE outlet inlet ]
applyRequestAction _ (ToDisconnect outletPath inletPath) nw = do
    pure  $ nw /\ [ ]
    -- pure $ nw /\ [ Disconnect link ]
    -- pure $ TODO: perform and remove cancelers
applyRequestAction _ (ToSendToInlet inletPath d) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendToInletE inlet d ]
applyRequestAction _ (ToSendToOutlet outletPath d) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendToOutletE outlet d ]
applyRequestAction _ (ToSendPeriodicallyToInlet inletPath period fn) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendPeriodicallyToInletE inlet period fn ]
applyRequestAction _ (ToStreamToInlet inletPath event) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ StreamToInletE inlet event ]
applyRequestAction _ (ToStreamToOutlet outletPath event) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ StreamToOutletE outlet event ]
applyRequestAction _ (ToSubscribeToInlet inletPath handler) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToInletE inlet handler ]
applyRequestAction _ (ToSubscribeToOutlet outletPath handler) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToOutletE outlet handler ]
applyRequestAction _ (ToSubscribeToNode nodePath inletsHandler outletsHandler) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToNodeE node inletsHandler outletsHandler ]


applyBuildAction
    :: forall d c n
     . Toolkit d c n
    -> BuildAction d c n
    -> Network d c n
    -> Step d c n
applyBuildAction _ (AddPatch p) nw = do
    pure $ Api.addPatch p nw /\ [ ]
applyBuildAction _ (AddNode node) nw = do
    nw' <- Api.addNode node nw
    pure $ nw' /\ [ ]
applyBuildAction tk (RemoveNode node) nw = do
    let (Node uuid _ _ _ _) = node
    inlets <- view (_nodeInlets uuid) nw # note (Err.ftfs $ UUID.uuid uuid)
    outlets <- view (_nodeOutlets uuid) nw # note (Err.ftfs $ UUID.uuid uuid)
    nw' /\ effs
        <- removeInlets inlets nw <∞> removeOutlets outlets
    nw'' <- Api.removeNode node nw'
    pure $ nw'' /\
        (effs <>
            [ CancelNodeSubscriptions node
            ]) -- FIXME: cancel all subscriptions
    where
        removeInlets :: forall f. Foldable f => f (Inlet d c) -> Network d c n -> Step d c n
        removeInlets inlets inNW =
            foldSteps inNW inlets $ applyBuildAction tk <<< RemoveInlet
        removeOutlets :: forall f. Foldable f => f (Outlet d c) -> Network d c n -> Step d c n
        removeOutlets outlets inNW =
            foldSteps inNW outlets $ applyBuildAction tk <<< RemoveOutlet
applyBuildAction _ (RemoveInlet inlet) nw = do
    nw' <- Api.removeInlet inlet nw
    pure $ nw' /\
        [ CancelInletSubscriptions inlet
        -- , ClearInletCancelers inlet
        -- , CancelNodeSubscriptions node
        -- , SubscribeNodeProcess node
        -- , InformNodeOnInletUpdates inlet node
        -- , SubscribeNodeUpdates node
        -- , SendActionOnInletUpdatesE inlet
        ] -- FIXME: cancel all subscriptions
applyBuildAction _ (RemoveOutlet outlet) nw = do
    -- FIXME: should call core API function
    nw' <- Api.removeOutlet outlet nw
    pure $ nw' /\ [ ] -- FIXME: cancel all subscriptions
applyBuildAction _ (ProcessWith node@(Node uuid _ _ _ _) processF) nw = do
    let newNode = Api.processWith processF node
        nw' = nw # setJust (_node uuid) newNode
    pure $ nw' /\ [ SubscribeNodeProcess newNode ]
applyBuildAction _ (AddInlet inlet@(Inlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.addInlet inlet nw
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnInletUpdates inlet node
        , SubscribeNodeUpdates node
        , SendActionOnInletUpdatesE inlet
        ]
applyBuildAction _ (AddOutlet outlet@(Outlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.addOutlet outlet nw
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnOutletUpdates outlet node
        , SubscribeNodeUpdates node
        , SendActionOnOutletUpdatesE outlet
        ]
applyBuildAction _ (Connect outlet inlet) nw = do
    pure $ nw /\ [ AddLinkE outlet inlet ]
applyBuildAction _ (AddLink link) nw = do
    nw' <- Api.addLink link nw
    pure $ nw' /\ []


applyInnerAction
    :: forall d c n
     . Toolkit d c n
    -> InnerAction d c n
    -> Network d c n
    -> Step d c n
applyInnerAction _ (Do effectful) nw =
    pure $ nw /\ [ DoE effectful ]
applyInnerAction _ (StoreNodeCanceler (Node uuid _ _ _ _) canceler) nw =
    let
        curNodeCancelers = Api.getNodeCancelers uuid nw
        newNodeCancelers = canceler : curNodeCancelers
    in
        pure $ Api.storeNodeCancelers uuid newNodeCancelers nw /\ []
applyInnerAction _ (ClearNodeCancelers (Node uuid _ _ _ _)) nw =
    pure $ Api.clearNodeCancelers uuid nw /\ []
applyInnerAction _ (StoreInletCanceler (Inlet uuid _ _ _) canceler) nw =
    let
        curInletCancelers = Api.getInletCancelers uuid nw
        newInletCancelers = canceler : curInletCancelers
    in
        pure $ Api.storeInletCancelers uuid newInletCancelers nw /\ []
applyInnerAction _ (ClearInletCancelers (Inlet uuid _ _ _)) nw =
    pure $ Api.clearInletCancelers uuid nw /\ []
applyInnerAction _ (StoreOutletCanceler (Outlet uuid _ _ _) canceler) nw =
    let
        curOutletCancelers = Api.getOutletCancelers uuid nw
        newOutletCancelers = canceler : curOutletCancelers
    in
        pure $ Api.storeOutletCancelers uuid newOutletCancelers nw /\ []
applyInnerAction _ (ClearOutletCancelers (Outlet uuid _ _ _)) nw =
    pure $ Api.clearOutletCancelers uuid nw /\ []
applyInnerAction _ (StoreLinkCanceler (Link uuid _) canceler) nw =
    let
        curLinkCancelers = Api.getLinkCancelers uuid nw
        newLinkCancelers = canceler : curLinkCancelers
    in
        pure $ Api.storeLinkCancelers uuid newLinkCancelers nw /\ []
applyInnerAction _ (ClearLinkCancelers (Link uuid _)) nw =
    pure $ Api.clearLinkCancelers uuid nw /\ []


performEffect -- TODO: move to a separate module
    :: forall d c n
     . Toolkit d c n -- TODO: check if it really needs toolkit
    -> (Action d c n -> Effect Unit)
    -> RpdEffect d c n
    -> Network d c n
    -> Effect Unit
performEffect _ pushAction (DoE effectful) nw = effectful nw
performEffect _ pushAction (AddPatchE alias) _ = do
    uuid <- UUID.new
    let path = Path.toPatch alias
    pushAction $ Build $ AddPatch $
        Patch
            (UUID.ToPatch uuid)
            path
            { nodes : Seq.empty
            , links : Seq.empty
            }
performEffect _ pushAction (AddNodeE patchPath nodeAlias n (NodeDef def)) nw = do
    uuid <- UUID.new
    flows <- Api.makeInletOutletsFlows
    let
        path = Path.nodeInPatch patchPath nodeAlias
        PushableFlow pushToInlets inletsFlow = flows.inlets
        PushableFlow pushToOutlets outletsFlow = flows.outlets
        newNode =
            Node
                (UUID.ToNode uuid)
                path
                n
                Withhold
                { inlets : Seq.empty
                , outlets : Seq.empty
                , inletsFlow : InletsFlow inletsFlow
                , outletsFlow : OutletsFlow outletsFlow
                , pushToInlets : PushToInlets pushToInlets
                , pushToOutlets : PushToOutlets pushToOutlets
                }
    pushAction $ Build $ AddNode newNode
    traverse_ (addInlet path) def.inlets
    traverse_ (addOutlet path) def.outlets
    pushAction $ Request $ ToProcessWith path def.process
    where
        addInlet path (InletAlias alias /\ c) =
            pushAction $ Request $ ToAddInlet path alias c
        addOutlet path (OutletAlias alias /\ c) =
            pushAction $ Request $ ToAddOutlet path alias c
performEffect toolkit pushAction (AddNextNodeE patchPath n (NodeDef def)) nw = do
    uuid <- UUID.new
    -- FIXME: use `show n`, maybe, or just take the part of the newly created node UUID?
    let shortHash = String.take 6 $ UUID.toRawString uuid
    performEffect toolkit pushAction (AddNodeE patchPath shortHash n (NodeDef def)) nw
performEffect _ pushAction (ProcessWithE node processF) nw = do
    pushAction $ Build $ ProcessWith node processF
performEffect _ pushAction (AddLinkE outlet inlet) nw = do
    uuid <- UUID.new
    let
        (Outlet ouuid _ _ { flow : outletFlow' }) = outlet
        (Inlet iuuid _ _ { push : pushToInlet' }) = inlet
        (OutletFlow outletFlow) = outletFlow'
        (PushToInlet pushToInlet) = pushToInlet'
        newLink = Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid }
    canceler :: Canceler <- E.subscribe outletFlow pushToInlet
    pushAction $ Build $ AddLink newLink
    pushAction $ Inner $ StoreLinkCanceler newLink canceler
performEffect _ pushAction (SubscribeNodeProcess node) nw = do
    canceler <- Api.setupNodeProcessFlow node nw
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (CancelNodeSubscriptions node@(Node uuid _ _ _ _)) nw = do
    _ <- Api.cancelNodeSubscriptions uuid nw
    pushAction $ Inner $ ClearNodeCancelers node
performEffect _ pushAction (CancelInletSubscriptions inlet@(Inlet uuid _ _ _ )) nw = do
    _ <- Api.cancelInletSubscriptions uuid nw
    pushAction $ Inner $ ClearInletCancelers inlet
performEffect _ pushAction (CancelOutletSubscriptions outlet@(Outlet uuid _ _ _ )) nw = do
    _ <- Api.cancelOutletSubscriptions uuid nw
    pushAction $ Inner $ ClearOutletCancelers outlet
performEffect _ pushAction (CancelLinkSubscriptions link@(Link uuid _)) nw = do
    _ <- Api.cancelLinkSubscriptions uuid nw
    pushAction $ Inner $ ClearLinkCancelers link
performEffect _ pushAction (AddInletE nodePath inletAlias c) _ = do
    uuid <- UUID.new
    flow <- makePushableFlow
    let
        path = Path.inletInNode nodePath inletAlias
        PushableFlow pushToInlet inletFlow = flow
        newInlet =
            Inlet
                (UUID.ToInlet uuid)
                path
                c
                { flow : InletFlow inletFlow
                , push : PushToInlet pushToInlet
                }
    pushAction $ Build $ AddInlet newInlet
    -- FIXME: pushAction $ CancelNodeSubscriptions
performEffect _ pushAction (InformNodeOnInletUpdates inlet node) _ = do
    canceler <- Api.informNodeOnInletUpdates inlet node
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (AddOutletE nodePath outletAlias c) _ = do
    uuid <- UUID.new
    flow <- makePushableFlow
    let
        path = Path.outletInNode nodePath outletAlias
        PushableFlow pushToOutlet outletFlow = flow
        newOutlet =
            Outlet
                (UUID.ToOutlet uuid)
                path
                c
                { flow : OutletFlow outletFlow
                , push : PushToOutlet pushToOutlet
                }
    pushAction $ Build $ AddOutlet newOutlet
    -- FIXME: pushAction $ CancelNodeSubscriptions
performEffect _ pushAction (InformNodeOnOutletUpdates outlet node) _ = do
    canceler :: Canceler <- Api.informNodeOnOutletUpdates outlet node
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SubscribeNodeUpdates node) _ = do
    canceler :: Canceler <-
        Api.subscribeNode node
            (const $ const $ const $ pure unit) -- FIXME: implement
            (const $ const $ const $ pure unit) -- FIXME: implement
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (SendToInletE inlet d) _ = do
    Api.sendToInlet inlet d
performEffect _ pushAction (SendToOutletE outlet d) _ =
    Api.sendToOutlet outlet d
performEffect _ pushAction (StreamToInletE inlet flow) _ = do
    canceler :: Canceler <- Api.streamToInlet inlet flow
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (StreamToOutletE outlet flow) _ = do
    canceler :: Canceler <- Api.streamToOutlet outlet flow
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SubscribeToInletE inlet handler) _ = do
    canceler :: Canceler <- Api.subscribeToInlet inlet handler
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SubscribeToOutletE outlet handler) _ = do
    canceler :: Canceler <- Api.subscribeToOutlet outlet handler
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction
    (SubscribeToNodeE node inletsHandler outletsHandler) _ = do
    canceler :: Canceler <- Api.subscribeNode node inletsHandler outletsHandler
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (SendActionOnInletUpdatesE inlet@(Inlet _ path _ { flow })) _ = do
    let (InletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotInletData inlet)
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SendActionOnOutletUpdatesE outlet@(Outlet _ path _ { flow })) _ = do
    let (OutletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotOutletData outlet)
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SendPeriodicallyToInletE inlet period fn) _ = do
    canceler :: Canceler <- Api.sendPeriodicallyToInlet inlet period fn
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SendPeriodicallyToOutletE outlet period fn) _ = do
    canceler :: Canceler <- Api.sendPeriodicallyToOutlet outlet period fn
    pushAction $ Inner $ StoreOutletCanceler outlet canceler


-- apply'
--     :: forall d c n
--      . Action d c n
--     -> (Action d c n -> Effect Unit)
--     -> Toolkit d c n
--     -> R.Network d c n
--     -> R.Rpd (R.Network d c n)
-- apply' Bang pushAction _ nw =
--     Rpd.subscribeAllInlets onInletData nw
--         </> Rpd.subscribeAllOutlets onOutletData
--     where
--         onInletData inletPath d =
--             pushAction $ GotInletData inletPath d
--         onOutletData outletPath d =
--             pushAction $ GotOutletData outletPath d
-- apply' (AddPatch alias) pushAction _ nw =
--     R.addPatch alias nw
--         -- FIXME: subscribe the nodes in the patch
-- apply' (AddNode patchPath alias n) pushAction _ nw =
--     Rpd.addNode patchPath alias n nw
--         -- FIXME: `onInletData`/`onOutletData` do not receive the proper state
--         --        of the network this way (do they need it?), but they should
--         --        (pass the current network state in the Process function?)
--         </> Rpd.subscribeNode nodePath
--                 (onNodeInletData nodePath)
--                 (onNodeOutletData nodePath)
--     where
--         nodePath = P.nodeInPatch patchPath alias
--         (patchAlias /\ nodeAlias) = P.explodeNodePath nodePath
--         -- addModel = pure <<< ((/\) model)
--         onNodeInletData nodePath (inletAlias /\ _ /\ d) =
--             pushAction $ GotInletData (P.toInlet patchAlias nodeAlias inletAlias) d
--         onNodeOutletData nodePath (outletAlias /\ _ /\ d) =
--             pushAction $ GotOutletData (P.toOutlet patchAlias nodeAlias outletAlias) d
-- apply' (AddInlet nodePath alias c) pushAction _ nw =
--     let
--         inletPath = P.inletInNode nodePath alias
--         onInletData d =
--             pushAction $ GotInletData inletPath d
--     in
--         Rpd.addInlet nodePath alias c nw
--             </> Rpd.subscribeInlet inletPath (R.InletHandler onInletData)
-- apply' (AddOutlet nodePath alias c) pushAction _ nw =
--     let
--         outletPath = P.outletInNode nodePath alias
--         onOutletData d =
--             pushAction $ GotOutletData outletPath d
--     in
--         Rpd.addOutlet nodePath alias c nw
--             </> Rpd.subscribeOutlet outletPath (R.OutletHandler onOutletData)
-- apply' (Connect { inlet : inletPath, outlet : outletPath }) _ _ nw =
--     Rpd.connect outletPath inletPath nw
-- apply' (Disconnect { inlet : inletPath, outlet : outletPath }) _ _ nw =
--     Rpd.disconnectTop outletPath inletPath nw
-- apply' _ _ _ nw = pure nw

