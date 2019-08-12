module Rpd.API.Action.Apply where

import Prelude
import Effect (Effect)

import Data.Maybe
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Sequence (empty) as Seq
import Data.Lens (view, setJust, set)
import Data.Array ((:))

import FRP.Event as E
import FRP.Event.Class (count) as E
import FRP.Event.Time as E

import Rpd.Util (PushableFlow(..), Canceler)
import Rpd.API
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
applyRequestAction _ (ToAddNodeByDef patchPath alias n def) nw =
    pure $ nw /\ [ AddNodeE patchPath alias n def ]
applyRequestAction _ (ToAddInlet nodePath alias c) nw =
    pure $ nw /\ [ AddInletE nodePath alias c ]
applyRequestAction _ (ToAddOutlet nodePath alias c) nw =
    pure $ nw /\ [ AddOutletE nodePath alias c ]
applyRequestAction _ (ToRemoveInlet inletPath) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    -- FIXME: should call core API function
    -- FIXME: join with `RemoveIntlet` build action
    nw' <- Right $ set (_inlet inletUuid) Nothing nw
    pure $ nw' /\ [ ] -- FIXME: cancel all subscriptions
applyRequestAction _ (ToRemoveOutlet outletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    -- FIXME: should call core API function
    -- FIXME: join with `RemoveOutlet` build action
    nw' <- Right $ set (_outlet outletUuid) Nothing nw
    pure $ nw' /\ [ ] -- FIXME: cancel all subscriptions
applyRequestAction _ (ToConnect outletPath inletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (RpdError "")
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (RpdError "")
    pure $ nw /\ [ AddLinkE outlet inlet ]
applyRequestAction _ (ToDisconnect outletPath inletPath) nw = do
    pure  $ nw /\ [ ]
    -- pure $ nw /\ [ Disconnect link ]
    -- pure $ TODO: perform and remove cancelers
applyRequestAction _ (ToSendToInlet inletPath d) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    (Inlet _ _ _ { push }) <- view (_inlet inletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendToInletE push d ]
applyRequestAction _ (ToSendToOutlet outletPath d) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    (Outlet _ _ _ { push }) <- view (_outlet outletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendToOutletE push d ]
applyRequestAction _ (ToSendPeriodicallyToInlet inletPath period fn) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    (Inlet _ _ _ { push }) <- view (_inlet inletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SendPeriodicallyToInletE push period fn ]
applyRequestAction _ (ToStreamToInlet inletPath event) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    (Inlet _ _ _ { push }) <- view (_inlet inletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ StreamToInletE push event ]
applyRequestAction _ (ToStreamToOutlet outletPath event) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    (Outlet _ _ _ { push }) <- view (_outlet outletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ StreamToOutletE push event ]
applyRequestAction _ (ToSubscribeToInlet inletPath handler) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    (Inlet _ _ _ { flow }) <- view (_inlet inletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToInletE flow handler ]
applyRequestAction _ (ToSubscribeToOutlet outletPath handler) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    (Outlet _ _ _ { flow }) <- view (_outlet outletUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToOutletE flow handler ]
applyRequestAction _ (ToSubscribeToNode nodePath inletsHandler outletsHandler) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    (Node _ _ _ _ { inletsFlow, outletsFlow }) <- view (_node nodeUuid) nw # note (RpdError "")
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ [ SubscribeToNodeE inletsFlow outletsFlow inletsHandler outletsHandler ]


applyBuildAction
    :: forall d c n
     . Toolkit d c n
    -> BuildAction d c n
    -> Network d c n
    -> Step d c n
applyBuildAction _ (AddPatch p) nw = do
    pure $ addPatch p nw /\ [ ]
applyBuildAction _ (AddNode node) nw = do
    nw' <- addNode node nw
    pure $ nw' /\ [ ]
applyBuildAction _ (RemoveInlet (Inlet inletUuid _ _ _)) nw = do
    -- FIXME: should call core API function
    nw' <- Right $ set (_inlet inletUuid) Nothing nw
    pure $ nw' /\ [ ] -- FIXME: cancel all subscriptions
applyBuildAction _ (RemoveOutlet (Outlet outletUuid _ _ _)) nw = do
    -- FIXME: should call core API function
    nw' <- Right $ set (_outlet outletUuid) Nothing nw
    pure $ nw' /\ [ ] -- FIXME: cancel all subscriptions
applyBuildAction _ (ProcessWith node@(Node uuid _ _ _ _) processF) nw = do
    let newNode = processWith processF node
        nw' = nw # setJust (_node uuid) newNode
    pure $ nw' /\ [ SubscribeNodeProcess newNode ]
applyBuildAction _ (AddInlet inlet@(Inlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (RpdError "")
    nw' <- addInlet inlet nw
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnInletUpdates inlet node
        , SubscribeNodeUpdates node
        , SendActionOnInletUpdatesE inlet
        ]
applyBuildAction _ (AddOutlet outlet@(Outlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (RpdError "")
    nw' <- addOutlet outlet nw
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnOutletUpdates outlet node
        , SubscribeNodeUpdates node
        , SendActionOnOutletUpdatesE outlet
        ]
applyBuildAction _ (AddLink link) nw = do
    nw' <- addLink link nw
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
        curNodeCancelers = getNodeCancelers uuid nw
        newNodeCancelers = canceler : curNodeCancelers
    in
        pure $ storeNodeCancelers uuid newNodeCancelers nw /\ []
applyInnerAction _ (ClearNodeCancelers (Node uuid _ _ _ _)) nw =
    pure $ clearNodeCancelers uuid nw /\ []
applyInnerAction _ (StoreOutletCanceler (Outlet uuid _ _ _) canceler) nw =
    let
        curOutletCancelers = getOutletCancelers uuid nw
        newOutletCancelers = canceler : curOutletCancelers
    in
        pure $ storeOutletCancelers uuid newOutletCancelers nw /\ []
applyInnerAction _ (StoreInletCanceler (Inlet uuid _ _ _) canceler) nw =
    let
        curInletCancelers = getInletCancelers uuid nw
        newInletCancelers = canceler : curInletCancelers
    in
        pure $ storeInletCancelers uuid newInletCancelers nw /\ []
applyInnerAction _ (StoreLinkCanceler (Link uuid _) canceler) nw =
    let
        curLinkCancelers = getLinkCancelers uuid nw
        newLinkCancelers = canceler : curLinkCancelers
    in
        pure $ storeLinkCancelers uuid newLinkCancelers nw /\ []


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
performEffect _ pushAction (AddNodeE patchPath nodeAlias n def) nw = do
    uuid <- UUID.new
    flows <- makeInletOutletsFlows
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
                { inlets : Seq.empty -- FIXME: load inlets / outlets from the `def`
                , outlets : Seq.empty -- FIXME: load inlets / outlets from the `def`
                , inletsFlow : InletsFlow inletsFlow
                , outletsFlow : OutletsFlow outletsFlow
                , pushToInlets : PushToInlets pushToInlets
                , pushToOutlets : PushToOutlets pushToOutlets
                }
    pushAction $ Build $ AddNode newNode
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
    canceler <- setupNodeProcessFlow node nw
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (CancelNodeSubscriptions node@(Node uuid _ _ _ _)) nw = do
    _ <- cancelNodeSubscriptions uuid nw
    pushAction $ Inner $ ClearNodeCancelers node
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
    canceler <- informNodeOnInletUpdates inlet node
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
    canceler :: Canceler <- informNodeOnOutletUpdates outlet node
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SubscribeNodeUpdates node) _ = do
    canceler :: Canceler <- subscribeNode node (const $ pure unit) (const $ pure unit)
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (SendToInletE (PushToInlet push) d) _ = do
    -- FIXME: should be the core `API` function
    push d -- TODO: consider pushing `GotInletData` action instead?
performEffect _ pushAction (SendToOutletE (PushToOutlet push) d) _ =
    -- FIXME: should be the core `API` function
    push d -- TODO: consider pushing `GotOutletData` action instead?
performEffect _ pushAction (StreamToInletE (PushToInlet push) flow) _ = do
    -- FIXME: should be the core `API` function
    canceler :: Canceler <- E.subscribe flow push
    pure unit
    -- TODO: pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (StreamToOutletE (PushToOutlet push) flow) _ = do
    -- FIXME: should be the core `API` function
    canceler :: Canceler <- E.subscribe flow push
    pure unit
    -- TODO: pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SubscribeToInletE (InletFlow flow) handler) _ = do
    canceler :: Canceler <- E.subscribe flow handler
    pure unit
    -- TODO: pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SubscribeToOutletE (OutletFlow flow) handler) _ = do
    canceler :: Canceler <- E.subscribe flow handler
    pure unit
    -- TODO: pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction
    (SubscribeToNodeE
        (InletsFlow inletsFlow) (OutletsFlow outletsFlow)
        inletsHandler outletsHandler
    ) _ = do
    iCanceler :: Canceler <- E.subscribe inletsFlow $
        (\((Path.ToInlet { inlet }) /\ uuid /\ d) -> inletsHandler inlet uuid d)
    oCanceler :: Canceler <- E.subscribe outletsFlow  $
        (\((Path.ToOutlet { outlet }) /\ uuid /\ d) -> outletsHandler outlet uuid d)
    pure unit
    -- FIXME: pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (SendActionOnInletUpdatesE inlet@(Inlet _ path _ { flow })) _ = do
    let (InletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotInletData inlet)
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SendActionOnOutletUpdatesE outlet@(Outlet _ path _ { flow })) _ = do
    let (OutletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotOutletData outlet)
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SendPeriodicallyToInletE (PushToInlet push) period fn) _ = do
    let intervalEvent = E.count $ E.interval period
    canceler :: Canceler <- E.subscribe intervalEvent $ push <<< fn
    pure unit
    -- FIXME: pushAction $ Inner $ StoreInletCanceler outlet canceler
performEffect _ pushAction (SendPeriodicallyToOutletE (PushToOutlet push) period fn) _ = do
    let intervalEvent = E.count $ E.interval period
    canceler :: Canceler <- E.subscribe intervalEvent $ push <<< fn
    pure unit
    -- FIXME: pushAction $ Inner $ StoreOutletCanceler outlet canceler



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

