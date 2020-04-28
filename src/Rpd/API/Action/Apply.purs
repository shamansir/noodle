module Rpd.API.Action.Apply where

import Prelude
import Effect (Effect)

import Data.Maybe
import Data.String (take) as String
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Sequence (empty, singleton, toUnfoldable) as Seq
import Data.Lens (view, setJust, set)
import Data.List (singleton, fromFoldable) as List
import Data.Array ((:)) as A
import Data.List ((:), List(..))
import Data.Foldable (class Foldable, foldr)
import Data.Traversable (traverse, traverse_)
import Data.Covered (Covered)
import Data.Covered (carry, fromEither, whenC, unpack, recover) as Covered
import Data.Foldable (fold)

import Debug.Trace as DT

import FRP.Event as E
import FRP.Event.Class (count) as E
import FRP.Event.Time as E

import FSM (doNothing, single, batch, join, join', join'', AndThen)
import FSM.Rollback (RollbackFSM)
import FSM.Rollback (followJoin) as Rollback

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
    )
import Rpd.Network
import Rpd.Process
import Rpd.Optics
import Rpd.Path as Path
import Rpd.Toolkit
import Rpd.UUID as UUID


type Step d c n = Covered RpdError (Network d c n) /\ Effect (AndThen (Action d c n))
type StepE d c n = Either RpdError ((Network d c n) /\ Effect (AndThen (Action d c n)))


infixl 1 next as <∞>


-- TODO: make an operator?
-- TODO: close to Actions sequensing?
next :: forall d c n. Step d c n -> (Network d c n -> Step d c n) -> Step d c n
next = Rollback.followJoin
    -- (nw /\ effs) <- stepA
    -- (nw' /\ effs') <- stepB nw
    -- pure $ nw' /\ (effs <> effs')


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
        ((Covered.carry $ initNW) /\ doNothing)
        foldable


apply
    :: forall d c n
     . Toolkit d c n
    -> (Action d c n -> Effect Unit)
    -> Action d c n
    -> Covered RpdError (Network d c n)
    -> Step d c n
apply toolkit pushAction action nw =
    apply' toolkit pushAction action $ Covered.recover nw


apply'
    :: forall d c n
     . Toolkit d c n
    -> (Action d c n -> Effect Unit)
    -> Action d c n
    -> Network d c n
    -> Step d c n
apply' toolkit pushAction action nw =
    proceed $ apply_ toolkit pushAction action nw
    where
        proceed :: StepE d c n -> Step d c n
        proceed = Covered.unpack <<< Covered.fromEither (nw /\ doNothing)


apply_
    :: forall d c n
     . Toolkit d c n
    -> (Action d c n -> Effect Unit)
    -> Action d c n
    -> Network d c n
    -> StepE d c n
apply_ _ _ NoOp nw = fine nw
apply_ toolkit _ (Inner innerAction) nw = applyInnerAction toolkit innerAction nw
apply_ toolkit _ (Request requestAction) nw = applyRequestAction toolkit requestAction nw
apply_ toolkit push (Build buildAction) nw =
    applyBuildAction toolkit (push <<< Data) buildAction nw
apply_ toolkit _ (Data dataAction) nw = applyDataAction toolkit dataAction nw


applyDataAction
    :: forall d c n
     . Toolkit d c n
    -> DataAction d c
    -> Network d c n
    -> StepE d c n
applyDataAction _ Bang nw =
    fine nw
applyDataAction _ (GotInletData _ _) nw =
    fine nw
applyDataAction _ (GotOutletData _ _) nw =
    fine nw
applyDataAction _ (SendToInlet inlet d) nw =
    pure $ nw /\ (Api.sendToInlet inlet d *> single NoOp)
applyDataAction _ (SendToOutlet outlet d) nw =
    pure $ nw /\ (Api.sendToOutlet outlet d *> single NoOp)
applyDataAction _ (StreamToInlet inlet flow) nw =
    pure $ nw /\ do
        canceler :: Canceler <- Api.streamToInlet inlet flow
        single $ Inner $ StoreInletCanceler inlet canceler
applyDataAction _ (StreamToOutlet outlet flow) nw =
    pure $ nw /\ do
        canceler :: Canceler <- Api.streamToOutlet outlet flow
        single $ Inner $ StoreOutletCanceler outlet canceler
applyDataAction _ (SendPeriodicallyToInlet inlet period fn) nw =
    pure $ nw /\ do
        canceler :: Canceler <- Api.sendPeriodicallyToInlet inlet period fn
        single $ Inner $ StoreInletCanceler inlet canceler


applyRequestAction
    :: forall d c n
     . Toolkit d c n
    -> RequestAction d c n
    -> Network d c n
    -> StepE d c n
applyRequestAction _ (ToAddPatch alias) nw =
    pure $ nw /\ do
        uuid <- UUID.new
        let path = Path.toPatch alias
        single $ Build $ AddPatch $
            Patch
                (UUID.ToPatch uuid)
                path
                { nodes : Seq.empty
                , links : Seq.empty
                }
applyRequestAction tk@(Toolkit _ getDef) (ToAddNode patchPath alias n) nw =
    applyRequestAction tk (ToAddNodeByDef patchPath alias n $ getDef n) nw
applyRequestAction tk@(Toolkit _ getDef) (ToAddNextNode patchPath n) nw = do
    applyRequestAction tk (ToAddNextNodeByDef patchPath n $ getDef n) nw
applyRequestAction _ (ToAddNodeByDef patchPath alias n (NodeDef def)) nw =
    pure $ nw /\ do
        uuid <- UUID.new
        flows <- Api.makeInletOutletsFlows
        let
            path = Path.nodeInPatch patchPath alias
            addInlet path (InletAlias alias /\ c) =
                Request $ ToAddInlet path alias c
            addOutlet path (OutletAlias alias /\ c) =
                Request $ ToAddOutlet path alias c
            PushableFlow pushToInlets inletsFlow = flows.inlets
            PushableFlow pushToOutlets outletsFlow = flows.outlets
            node =
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
        join''
            $ (single $ Build $ AddNode node)
            : (batch $ addInlet path <$> def.inlets)
            : (batch $ addOutlet path <$> def.outlets)
            : (single $ Request $ ToProcessWith path def.process)
            : Nil
        {-
        , do
            flows <- Api.makeInletOutletsFlows
            let
                PushableFlow pushToInlets inletsFlow = flows.inlets
                PushableFlow pushToOutlets outletsFlow = flows.outlets
            pure $ Batch $ (addInlet path <$> def.inlets) <> (addOutlet path <$> def.outlets)
        -}
applyRequestAction _ (ToAddNextNodeByDef patchPath n def) nw = do
    pure $ nw /\ do
        uuid <- UUID.new
        let shortHash = String.take 6 $ UUID.toRawString uuid
        single $ Request $ ToAddNodeByDef patchPath shortHash n def
applyRequestAction tk (ToRemoveNode nodePath) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    pure $ nw /\ (single $ Build $ RemoveNode node)
applyRequestAction _ (ToAddInlet nodePath alias c) nw =
    pure $ nw /\ do
        uuid <- UUID.new
        flow <- makePushableFlow
        let
            path = Path.inletInNode nodePath alias
            PushableFlow pushToInlet inletFlow = flow
            newInlet =
                Inlet
                    (UUID.ToInlet uuid)
                    path
                    c
                    { flow : InletFlow inletFlow
                    , push : PushToInlet pushToInlet
                    }
        single $ Build $ AddInlet newInlet
applyRequestAction _ (ToAddOutlet nodePath alias c) nw =
    pure $ nw /\ do
        uuid <- UUID.new
        flow <- makePushableFlow
        let
            path = Path.outletInNode nodePath alias
            PushableFlow pushToOutlet outletFlow = flow
            newOutlet =
                Outlet
                    (UUID.ToOutlet uuid)
                    path
                    c
                    { flow : OutletFlow outletFlow
                    , push : PushToOutlet pushToOutlet
                    }
        single $ Build $ AddOutlet newOutlet
applyRequestAction tk (ToRemoveInlet inletPath) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    pure $ nw /\ (single $ Build $ RemoveInlet inlet)
applyRequestAction tk (ToRemoveOutlet outletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    pure $ nw /\ (single $ Build $ RemoveOutlet outlet)
applyRequestAction _ (ToProcessWith nodePath processF) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    pure $ nw /\ (single $ Build $ ProcessWith node processF)
applyRequestAction _ (ToConnect outletPath inletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    pure $ nw /\ do
        uuid <- UUID.new
        let
            (Outlet ouuid _ _ { flow : outletFlow' }) = outlet
            (Inlet iuuid _ _ { push : pushToInlet' }) = inlet
            (OutletFlow outletFlow) = outletFlow'
            (PushToInlet pushToInlet) = pushToInlet'
            newLink = Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid }
        canceler :: Canceler <- E.subscribe outletFlow pushToInlet
        batch $
               (Build $ AddLink newLink)
             : (Inner $ StoreLinkCanceler newLink canceler)
             : Nil
applyRequestAction _ (ToDisconnect outletPath inletPath) nw = do
    fine nw
    -- pure $ nw /\ [ Disconnect link ]
    -- pure $ TODO: perform and remove cancelers
applyRequestAction _ (ToSendToInlet inletPath d) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    -- FIXME: use SendToInlet data action?
    pure $ nw /\ (single $ Data $ SendToInlet inlet d)
applyRequestAction _ (ToSendToOutlet outletPath d) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (single $ Data $ SendToOutlet outlet d)
applyRequestAction _ (ToSendPeriodicallyToInlet inletPath period fn) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (single $ Data $ SendPeriodicallyToInlet inlet period fn)
applyRequestAction _ (ToStreamToInlet inletPath event) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (single $ Data $ StreamToInlet inlet event)
applyRequestAction _ (ToStreamToOutlet outletPath event) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (single $ Data $ StreamToOutlet outlet event)
applyRequestAction _ (ToSubscribeToInlet inletPath handler) nw = do
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ do
        canceler :: Canceler <- Api.subscribeToInlet inlet handler
        single $ Inner $ StoreInletCanceler inlet canceler
applyRequestAction _ (ToSubscribeToOutlet outletPath handler) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ do
        canceler :: Canceler <- Api.subscribeToOutlet outlet handler
        single $ Inner $ StoreOutletCanceler outlet canceler
applyRequestAction _ (ToSubscribeToNode nodePath inletsHandler outletsHandler) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ do
        canceler :: Canceler <- Api.subscribeNode node inletsHandler outletsHandler
        single $ Inner $ StoreNodeCanceler node canceler


applyBuildAction
    :: forall d c n
     . Toolkit d c n
    -> (DataAction d c -> Effect Unit)
    -> BuildAction d c n
    -> Network d c n
    -> StepE d c n
applyBuildAction _ _ (AddPatch p) nw =
    fine $ Api.addPatch p nw
applyBuildAction _ _ (AddNode node) nw =
    withE $ Api.addNode node nw
        {--
        [
            All subscriptions
        ]
        --}
applyBuildAction tk _ (RemoveNode node) nw = do
    withE $ Api.removeNode node nw
    {- FIXME: bring back, RemoveNode would do that in order:
              remove inlets, remove outlets, so cancel their subscriptions
              then remove the node itself
    let (Node uuid _ _ _ _) = node
    inlets <- view (_nodeInlets uuid) nw # note (Err.ftfs $ UUID.uuid uuid)
    outlets <- view (_nodeOutlets uuid) nw # note (Err.ftfs $ UUID.uuid uuid)
    nw' /\ effs
        <- removeInlets inlets nw <∞> removeOutlets outlets
    nw'' <- Api.removeNode node nw'
    pure $ nw'' /\
        (effs <>
            [ CancelNodeSubscriptions node
            ])
    where
        removeInlets :: forall f. Foldable f => f (Inlet d c) -> Network d c n -> Step d c n
        removeInlets inlets inNW =
            foldSteps inNW inlets $ applyBuildAction tk <<< RemoveInlet
        removeOutlets :: forall f. Foldable f => f (Outlet d c) -> Network d c n -> Step d c n
        removeOutlets outlets inNW =
            foldSteps inNW outlets $ applyBuildAction tk <<< RemoveOutlet
    -}
applyBuildAction _ _ (RemoveInlet inlet@(Inlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.removeInlet inlet nw
    pure $ nw' /\ do
        _ <- Api.cancelInletSubscriptions uuid nw'
        -- _ <- Api.cancelNodeSubscriptions nodeUuid nw'
        nodeProcessCanceler <- Api.setupNodeProcessFlow node nw'
        single $ Inner $ StoreNodeCanceler node nodeProcessCanceler
applyBuildAction _ _ (RemoveOutlet outlet) nw = do
    nw' <- Api.removeOutlet outlet nw
    pure $ nw' /\
        (doNothing {- CancelOutletSubscriptions outlet -})
applyBuildAction _ _ (ProcessWith node@(Node uuid _ _ _ _) processF) nw =
    let newNode = Api.processWith processF node
        nw' = nw # setJust (_node uuid) newNode
    in
        pure $ nw' /\ do
            processCanceler <- Api.setupNodeProcessFlow newNode nw'
            single $ Inner $ StoreNodeCanceler newNode processCanceler
applyBuildAction _ pushAction (AddInlet inlet@(Inlet uuid path _ { flow })) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.addInlet inlet nw
    pure $ nw' /\ do
        _ <- Api.cancelNodeSubscriptions nodeUuid nw
        processCanceler <- Api.setupNodeProcessFlow node nw
        inletUpdatesCanceler <- Api.informNodeOnInletUpdates inlet node
        nodeUpdatesCanceler <-
            Api.subscribeNode node
                (const $ const $ const $ pure unit) -- FIXME: implement
                (const $ const $ const $ pure unit) -- FIXME: implement
        let (InletFlow flow') = flow
        sendValuesCanceler <- E.subscribe flow' (pushAction <<< GotInletData inlet)
        batch $ Inner
            <$> ClearNodeCancelers node
            : StoreNodeCanceler node processCanceler
            : StoreInletCanceler inlet inletUpdatesCanceler
            : StoreNodeCanceler node nodeUpdatesCanceler
            : StoreInletCanceler inlet sendValuesCanceler
            : Nil
applyBuildAction _ pushAction (AddOutlet outlet@(Outlet uuid path _ { flow })) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.addOutlet outlet nw
    pure $ nw' /\ do
        _ <- Api.cancelNodeSubscriptions nodeUuid nw
        processCanceler <- Api.setupNodeProcessFlow node nw
        outletUpdatesCanceler <- Api.informNodeOnOutletUpdates outlet node
        nodeUpdatesCanceler <-
            Api.subscribeNode node
                (const $ const $ const $ pure unit) -- FIXME: implement
                (const $ const $ const $ pure unit) -- FIXME: implement
        let (OutletFlow flow') = flow
        sendValuesCanceler <- E.subscribe flow' (pushAction <<< GotOutletData outlet)
        batch $ Inner
            <$> ClearNodeCancelers node
            : StoreNodeCanceler node processCanceler
            : StoreOutletCanceler outlet outletUpdatesCanceler
            : StoreNodeCanceler node nodeUpdatesCanceler
            : StoreOutletCanceler outlet sendValuesCanceler
            : Nil
applyBuildAction _ _ (Connect (Outlet ouuid _ _ _) (Inlet iuuid _ _ _)) nw = do
    pure $ nw /\ do
        uuid <- UUID.new
        let newLink = Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid }
        single $ Build $ AddLink newLink
applyBuildAction _ _ (AddLink link) nw =
    withE $ Api.addLink link nw -- TODO: subscriptions


applyInnerAction
    :: forall d c n
     . Toolkit d c n
    -> InnerAction d c n
    -> Network d c n
    -> StepE d c n
applyInnerAction _ (Do effectful) nw =
    pure $ nw /\ (effectful nw *> single NoOp)
applyInnerAction _ (StoreNodeCanceler (Node uuid _ _ _ _) canceler) nw =
    let
        curNodeCancelers = Api.getNodeCancelers uuid nw
        newNodeCancelers = canceler A.: curNodeCancelers
    in
        fine $ Api.storeNodeCancelers uuid newNodeCancelers nw
applyInnerAction _ (ClearNodeCancelers (Node uuid _ _ _ _)) nw =
    fine $ Api.clearNodeCancelers uuid nw
applyInnerAction _ (StoreInletCanceler (Inlet uuid _ _ _) canceler) nw =
    let
        curInletCancelers = Api.getInletCancelers uuid nw
        newInletCancelers = canceler A.: curInletCancelers
    in
        fine $ Api.storeInletCancelers uuid newInletCancelers nw
applyInnerAction _ (ClearInletCancelers (Inlet uuid _ _ _)) nw =
    fine $ Api.clearInletCancelers uuid nw
applyInnerAction _ (StoreOutletCanceler (Outlet uuid _ _ _) canceler) nw =
    let
        curOutletCancelers = Api.getOutletCancelers uuid nw
        newOutletCancelers = canceler A.: curOutletCancelers
    in
        fine $ Api.storeOutletCancelers uuid newOutletCancelers nw
applyInnerAction _ (ClearOutletCancelers (Outlet uuid _ _ _)) nw =
    fine $ Api.clearOutletCancelers uuid nw
applyInnerAction _ (StoreLinkCanceler (Link uuid _) canceler) nw =
    let
        curLinkCancelers = Api.getLinkCancelers uuid nw
        newLinkCancelers = canceler A.: curLinkCancelers
    in
        fine $ Api.storeLinkCancelers uuid newLinkCancelers nw
applyInnerAction _ (ClearLinkCancelers (Link uuid _)) nw =
    fine $ Api.clearLinkCancelers uuid nw


fine :: forall d c n. Network d c n -> StepE d c n
fine nw = pure $ nw /\ doNothing


withE :: forall d c n. Either RpdError (Network d c n) -> StepE d c n
withE e = e <#> (flip (/\) $ doNothing)


{-
performEffect -- TODO: move to a separate module
    :: forall d c n
     . Toolkit d c n -- TODO: check if it really needs toolkit
    -> (Action d c n -> Effect Unit)
    -> RpdEffect d c n
    -> Network d c n
    -> Effect Unit
performEffect _ pushAction (DoE effectful) nw = effectful nw
performEffect _ pushAction (AddPatchE alias) _ = MOVED
performEffect _ pushAction (AddNodeE patchPath nodeAlias n (NodeDef def)) _ = MOVED
performEffect toolkit pushAction (AddNextNodeE patchPath n (NodeDef def)) nw = MOVED
performEffect _ pushAction (ProcessWithE node processF) _ = do
    pushAction $ Build $ ProcessWith node processF
performEffect _ pushAction (AddLinkE outlet inlet) _ =  MOVED
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
performEffect _ pushAction (AddInletE nodePath inletAlias c) _ = MOVED
performEffect _ pushAction (InformNodeOnInletUpdates inlet node) _ = do
    canceler <- Api.informNodeOnInletUpdates inlet node
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (AddOutletE nodePath outletAlias c) _ = MOVED
performEffect _ pushAction (InformNodeOnOutletUpdates outlet node) _ = do
    canceler :: Canceler <- Api.informNodeOnOutletUpdates outlet node
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SubscribeNodeUpdates node) _ = do
    canceler :: Canceler <-
        Api.subscribeNode node
            (const $ const $ const $ pure unit) -- FIXME: implement
            (const $ const $ const $ pure unit) -- FIXME: implement
    pushAction $ Inner $ StoreNodeCanceler node canceler
performEffect _ pushAction (SendToInletE inlet d) _ = MOVED
performEffect _ pushAction (SendToOutletE outlet d) _ = MOVED
performEffect _ pushAction (StreamToInletE inlet flow) _ = MOVED
performEffect _ pushAction (StreamToOutletE outlet flow) _ = MOVED
performEffect _ pushAction (SubscribeToInletE inlet handler) _ = MOVED
performEffect _ pushAction (SubscribeToOutletE outlet handler) _ = MOVED
performEffect _ pushAction
    (SubscribeToNodeE node inletsHandler outletsHandler) _ = MOVED
performEffect _ pushAction (SendActionOnInletUpdatesE inlet@(Inlet _ path _ { flow })) _ = do
    let (InletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotInletData inlet)
    pushAction $ Inner $ StoreInletCanceler inlet canceler
performEffect _ pushAction (SendActionOnOutletUpdatesE outlet@(Outlet _ path _ { flow })) _ = do
    let (OutletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (pushAction <<< Data <<< GotOutletData outlet)
    pushAction $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ pushAction (SendPeriodicallyToInletE inlet period fn) _ = MOVED
performEffect _ pushAction (SendPeriodicallyToOutletE outlet period fn) _ = MOVED
-}


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

