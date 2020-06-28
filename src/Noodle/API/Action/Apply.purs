module Noodle.API.Action.Apply where

import Prelude
import Effect (Effect)

import Data.Maybe
import Data.String (take) as String
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Sequence (empty, singleton, toUnfoldable, length, filter) as Seq
import Data.Sequence.Extra (catMaybes) as Seq
import Data.Lens (view, setJust, set, preview, _Just)
import Data.List (singleton, fromFoldable) as List
import Data.Array ((:)) as A
import Data.List ((:), List(..))
import Data.Foldable (class Foldable, foldr)
import Data.Traversable (traverse, traverse_)
import Data.Covered (Covered)
import Data.Covered (carry, fromEither, whenC, unpack, recover) as Covered
import Data.Foldable (fold)

import FRP.Event as E
import FRP.Event.Class (count) as E
import FRP.Event.Time as E

import FSM (doNothing, single, batch, just)
import FSM.Rollback (follow, followJoin, fine, foldUpdate) as Covered

import Noodle.Util (PushableFlow(..), Canceler)
import Noodle.API as Api
import Noodle.API (uuidByPath, makePushableFlow)
import Noodle.API.Errors (NoodleError)
import Noodle.API.Errors as Err
import Noodle.API.Action
    ( Action(..)
    , InnerAction(..)
    , RequestAction(..)
    , BuildAction(..)
    , DataAction(..)
    )
import Noodle.Network
import Noodle.Process
import Noodle.Optics
import Noodle.Path as Path
import Noodle.Toolkit
import Noodle.UUID as UUID


type Step d c n = Covered NoodleError (Network d c n) /\ List (Effect (Action d c n))
type StepE d c n = Either NoodleError (Network d c n /\ List (Effect (Action d c n)))


infixl 1 next as <∞>


-- TODO: make an operator?
-- TODO: close to Actions sequensing?
next :: forall d c n. Step d c n -> (Network d c n -> Step d c n) -> Step d c n
next = Covered.followJoin
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
    -> Covered NoodleError (Network d c n)
    -> Step d c n
apply toolkit push action coveredNw =
    apply' toolkit push action $ Covered.recover coveredNw


apply'
    :: forall d c n
     . Toolkit d c n
    -> (Action d c n -> Effect Unit)
    -> Action d c n
    -> Network d c n
    -> Step d c n
apply' toolkit push action nw =
    proceed $ apply_ toolkit push action nw
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
    pure $ nw /\ (just $ Api.sendToInlet inlet d *> pure NoOp)
applyDataAction _ (SendToOutlet outlet d) nw =
    pure $ nw /\ (just $ Api.sendToOutlet outlet d *> pure NoOp)
applyDataAction _ (StreamToInlet inlet flow) nw =
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.streamToInlet inlet flow
        pure $ Inner $ StoreInletCanceler inlet canceler)
applyDataAction _ (StreamToOutlet outlet flow) nw =
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.streamToOutlet outlet flow
        pure $ Inner $ StoreOutletCanceler outlet canceler)
applyDataAction _ (SendPeriodicallyToInlet inlet period fn) nw =
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.sendPeriodicallyToInlet inlet period fn
        pure $ Inner $ StoreInletCanceler inlet canceler)


applyRequestAction
    :: forall d c n
     . Toolkit d c n
    -> RequestAction d c n
    -> Network d c n
    -> StepE d c n
applyRequestAction _ (ToAddPatch alias) nw =
    pure $ nw /\ (just $ do
        uuid <- UUID.new
        let path = Path.toPatch alias
        pure $ Build $ AddPatch $
            Patch
                (UUID.ToPatch uuid)
                path
                { nodes : Seq.empty
                , links : Seq.empty
                })
applyRequestAction tk@(Toolkit _ getDef) (ToAddNode patchPath alias n) nw =
    applyRequestAction tk (ToAddNodeByDef patchPath alias n $ getDef n) nw
applyRequestAction tk@(Toolkit _ getDef) (ToAddNextNode patchPath n) nw = do
    applyRequestAction tk (ToAddNextNodeByDef patchPath n $ getDef n) nw
applyRequestAction _ (ToAddNodeByDef patchPath alias n (NodeDef def)) nw =
    let path = Path.nodeInPatch patchPath alias
    in pure $ nw /\ (
        ( just $ do
            uuid <- UUID.new
            flows <- Api.makeInletOutletsFlows
            let
                PushableFlow pushToInlets inletsFlow = flows.inlets
                PushableFlow pushToOutlets outletsFlow = flows.outlets
                node =
                    Node
                        (UUID.ToNode uuid)
                        path
                        n
                        Auto
                        Withhold
                        { inlets : Seq.empty
                        , outlets : Seq.empty
                        , inletsFlow : InletsFlow inletsFlow
                        , outletsFlow : OutletsFlow outletsFlow
                        , pushToInlets : PushToInlets pushToInlets
                        , pushToOutlets : PushToOutlets pushToOutlets
                        }
            pure $ Build $ AddNode node )
        <> (batch $ addInlet path <$> def.inlets)
        <> (batch $ addOutlet path <$> def.outlets)
        <> (single $ Request $ ToProcessWith path def.process)
    )
    where
        addInlet path (InletAlias alias /\ c) =
            Request $ ToAddInlet path alias c
        addOutlet path (OutletAlias alias /\ c) =
            Request $ ToAddOutlet path alias c
applyRequestAction _ (ToAddNextNodeByDef patchPath n def) nw = do
    pure $ nw /\ (just $ do
        uuid <- UUID.new
        let shortHash = String.take 6 $ UUID.toRawString uuid
        pure $ Request $ ToAddNodeByDef patchPath shortHash n def)
applyRequestAction tk (ToRemoveNode nodePath) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    patchUuid <- uuidByPath UUID.toPatch (Path.getPatchPath $ Path.lift nodePath) nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    patch <- view (_patch patchUuid) nw # note (Err.ftfs $ UUID.uuid patchUuid)
    -- FIXME: shouldn't `links`  be removed within the calls to remove inlets (/outlets)?
    let
        inletsUuids = view (_node nodeUuid <<< _Just <<< _nodeInlets) nw
        outletsUuids = view (_node nodeUuid <<< _Just <<< _nodeOutlets) nw
        inlets = Seq.catMaybes $ (\uuid -> view (_inlet uuid) nw) <$> inletsUuids
        outlets = Seq.catMaybes $ (\uuid -> view (_outlet uuid) nw) <$> outletsUuids
        allLinksUuids = view (_patch patchUuid <<< _Just <<< _patchLinks) nw
        allLinks = Seq.catMaybes $ (\uuid -> view (_link uuid) nw) <$> allLinksUuids
        filteredLinks =
            Seq.catMaybes
             $ ((\inletUuid outletUuid link@(Link _ { inlet, outlet }) ->
                    if inlet == inletUuid || outlet == outletUuid
                    then Just link else Nothing)
            <$> inletsUuids <*> outletsUuids <*> allLinks)
    pure $ nw /\
            (  (batch $ Seq.toUnfoldable $ Build <<< RemoveLink <$> filteredLinks)
            <> (batch $ Seq.toUnfoldable $ Build <<< RemoveInlet <$> inlets)
            <> (batch $ Seq.toUnfoldable $ Build <<< RemoveOutlet <$> outlets)
            <> (single $ Build $ RemoveNode node)
            )
applyRequestAction _ (ToAddInlet nodePath alias c) nw =
    pure $ nw /\ (just $ do
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
        pure $ Build $ AddInlet newInlet)
applyRequestAction _ (ToAddOutlet nodePath alias c) nw =
    pure $ nw /\ (just $ do
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
        pure $ Build $ AddOutlet newOutlet)
applyRequestAction tk (ToRemoveInlet inletPath) nw = do
    patchUuid <- uuidByPath UUID.toPatch (Path.getPatchPath $ Path.lift inletPath) nw
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    inlet <- view (_inlet inletUuid) nw # note (Err.ftfs $ UUID.uuid inletUuid)
    let
        allLinksUuids = view (_patch patchUuid <<< _Just <<< _patchLinks) nw
        allLinks = Seq.catMaybes $ (\uuid -> view (_link uuid) nw) <$> allLinksUuids
        inletLinks =
            allLinks # Seq.filter (\(Link _ { inlet }) -> inlet == inletUuid)
    pure $ nw /\
            (  (batch $ Seq.toUnfoldable $ Build <<< RemoveLink <$> inletLinks)
            <> (single $ Build $ RemoveInlet inlet)
            )
applyRequestAction tk (ToRemoveOutlet outletPath) nw = do
    patchUuid <- uuidByPath UUID.toPatch (Path.getPatchPath $ Path.lift outletPath) nw
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    let
        allLinksUuids = view (_patch patchUuid <<< _Just <<< _patchLinks) nw
        allLinks = Seq.catMaybes $ (\uuid -> view (_link uuid) nw) <$> allLinksUuids
        outletLinks =
            allLinks # Seq.filter (\(Link _ { outlet }) -> outlet == outletUuid)
    pure $ nw /\
            (  (batch $ Seq.toUnfoldable $ Build <<< RemoveLink <$> outletLinks)
            <> (single $ Build $ RemoveOutlet outlet)
            )
applyRequestAction _ (ToProcessWith nodePath processF) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    pure $ nw /\ (single $ Build $ ProcessWith node processF)
applyRequestAction _ (ToConnect outletPath inletPath) nw = do
    ouuid <- uuidByPath UUID.toOutlet outletPath nw
    iuuid <- uuidByPath UUID.toInlet inletPath nw
    pure $ nw /\ (just $ do
        uuid <- UUID.new
        pure $ Build $ Connect
            $ Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid })
applyRequestAction _ (ToDisconnect outletPath inletPath) nw = do
    patchUuid <- uuidByPath UUID.toPatch (Path.getPatchPath $ Path.lift outletPath) nw
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    let
        allLinksUuids = view (_patch patchUuid <<< _Just <<< _patchLinks) nw
        allLinks = Seq.catMaybes $ (\uuid -> view (_link uuid) nw) <$> allLinksUuids
        linksBetween =
            allLinks
                # Seq.filter
                    (\(Link _ { inlet, outlet }) -> inlet == inletUuid && outlet == outletUuid)
    -- FIXME: perform all cancelers
    pure $ nw /\ (batch $ Seq.toUnfoldable $ Build <<< RemoveLink <$> linksBetween)
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
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.subscribeToInlet inlet handler
        pure $ Inner $ StoreInletCanceler inlet canceler)
applyRequestAction _ (ToSubscribeToOutlet outletPath handler) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    outlet <- view (_outlet outletUuid) nw # note (Err.ftfs $ UUID.uuid outletUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.subscribeToOutlet outlet handler
        pure $ Inner $ StoreOutletCanceler outlet canceler)
applyRequestAction _ (ToSubscribeToNode nodePath inletsHandler outletsHandler) nw = do
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    -- TODO: adapt / check the data with the channel instance? or do it in the caller?
    pure $ nw /\ (just $ do
        canceler :: Canceler <- Api.subscribeNode node inletsHandler outletsHandler
        pure $ Inner $ StoreNodeCanceler node canceler)


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
applyBuildAction tk _ (RemoveNode node@(Node uuid _ _ _ _ _)) nw = do
    nw' <- Api.removeNode node nw
    pure $ nw' /\ (just $ do
        _ <- Api.cancelNodeSubscriptions uuid nw'
        pure $ Inner $ ClearNodeCancelers node)
applyBuildAction _ _ (RemoveInlet inlet@(Inlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.removeInlet inlet nw
    pure $ nw' /\ (just $ do
        _ <- Api.cancelInletSubscriptions uuid nw'
        _ <- Api.cancelNodeSubscriptions nodeUuid nw'
        nodeProcessCanceler <- Api.setupNodeProcessFlow node nw'
        pure $ Inner $ StoreNodeCanceler node nodeProcessCanceler)
applyBuildAction _ _ (RemoveOutlet outlet@(Outlet uuid path _ _)) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    node <- view (_node nodeUuid) nw # note (Err.ftfs $ UUID.uuid nodeUuid)
    nw' <- Api.removeOutlet outlet nw
    pure $ nw' /\ (just $ do
        _ <- Api.cancelOutletSubscriptions uuid nw'
        _ <- Api.cancelNodeSubscriptions nodeUuid nw'
        nodeProcessCanceler <- Api.setupNodeProcessFlow node nw'
        pure $ Inner $ StoreNodeCanceler node nodeProcessCanceler)
applyBuildAction _ _ (ProcessWith node@(Node uuid _ _ _ _ _) processF) nw =
    let newNode = Api.processWith processF node
        nw' = nw # setJust (_node uuid) newNode
    in
        pure $ nw' /\ (just $ do
            _ <- Api.cancelNodeSubscriptions uuid nw'
            processCanceler <- Api.setupNodeProcessFlow newNode nw'
            pure $ Inner $ StoreNodeCanceler newNode processCanceler)
applyBuildAction _ push (AddInlet inlet@(Inlet uuid path _ { flow })) nw = do
    nw' <- Api.addInlet inlet nw
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw'
    node <- view (_node nodeUuid) nw' # note (Err.ftfs $ UUID.uuid nodeUuid)
    let (InletFlow flow') = flow
    pure $ nw' /\
        -- FIXME: all those should be separate Actions without the API code
        -- performed afterwards, not before?
        ( (do
            _ <- Api.cancelNodeSubscriptions nodeUuid nw'
            pure $ Inner $ ClearNodeCancelers node)
        : (do
            processCanceler <- Api.setupNodeProcessFlow node nw'
            pure $ Inner $ StoreNodeCanceler node processCanceler)
        : (do
            inletUpdatesCanceler <- Api.informNodeOnInletUpdates inlet node
            pure $ Inner $ StoreInletCanceler inlet inletUpdatesCanceler)
        : (do
            nodeUpdatesCanceler <-
                Api.subscribeNode node
                    (const $ const $ const $ pure unit) -- FIXME: implement
                    (const $ const $ const $ pure unit) -- FIXME: implement
            pure $ Inner $ StoreNodeCanceler node nodeUpdatesCanceler)
        : (do
            sendValuesCanceler <- E.subscribe flow' (push <<< GotInletData inlet)
            pure $ Inner $ StoreInletCanceler inlet sendValuesCanceler)
        : Nil
        )
applyBuildAction _ push (AddOutlet outlet@(Outlet uuid path _ { flow })) nw = do
    nw' <- Api.addOutlet outlet nw
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- uuidByPath UUID.toNode nodePath nw'
    node <- view (_node nodeUuid) nw' # note (Err.ftfs $ UUID.uuid nodeUuid)
    let (OutletFlow flow') = flow
    pure $ nw' /\
       -- FIXME: all those should be separate Actions without the API code
        -- performed afterwards, not before?
        ( (do
            _ <- Api.cancelNodeSubscriptions nodeUuid nw'
            pure $ Inner $ ClearNodeCancelers node)
        : (do
            processCanceler <- Api.setupNodeProcessFlow node nw'
            pure $ Inner $ StoreNodeCanceler node processCanceler)
        : (do
            outletUpdatesCanceler <- Api.informNodeOnOutletUpdates outlet node
            pure $ Inner $ StoreOutletCanceler outlet outletUpdatesCanceler)
        : (do
            nodeUpdatesCanceler <-
                Api.subscribeNode node
                    (const $ const $ const $ pure unit) -- FIXME: implement
                    (const $ const $ const $ pure unit) -- FIXME: implement
            pure $ Inner $ StoreNodeCanceler node nodeUpdatesCanceler)
        : (do
            sendValuesCanceler <- E.subscribe flow' (push <<< GotOutletData outlet)
            pure $ Inner $ StoreOutletCanceler outlet sendValuesCanceler)
        : Nil
        )
applyBuildAction _ _ (Connect link@(Link _ { outlet : ouuid, inlet : iuuid })) nw = do
    outlet <- view (_outlet ouuid) nw # note (Err.ftfs $ UUID.uuid ouuid)
    inlet <- view (_inlet iuuid) nw # note (Err.ftfs $ UUID.uuid iuuid)
    pure $ nw /\
        ( (pure $ Build $ AddLink link)
         -- FIXME: is it right to add link and connect at the same time?
         -- Do not expose `addLink` then
        : (do
            let
                (Outlet ouuid _ _ { flow : outletFlow' }) = outlet
                (Inlet iuuid _ _ { push : pushToInlet' }) = inlet
                (OutletFlow outletFlow) = outletFlow'
                (PushToInlet pushToInlet) = pushToInlet'
            canceler :: Canceler <- E.subscribe outletFlow pushToInlet
            pure $ Inner $ StoreLinkCanceler link canceler)
        : Nil
        )
applyBuildAction _ _ (AddLink link) nw = do
    withE $ Api.addLink link nw
applyBuildAction _ _ (RemoveLink link@(Link linkUuid _)) nw = do
    nw' <- Api.removeLink link nw
    pure $ nw' /\ (just $ do
        -- _ <- Api.cancelOutletSubscriptions outlet nw
        _ <- Api.cancelLinkSubscriptions linkUuid nw
        pure NoOp)


applyInnerAction
    :: forall d c n
     . Toolkit d c n
    -> InnerAction d c n
    -> Network d c n
    -> StepE d c n
applyInnerAction _ (StoreNodeCanceler (Node uuid _ _ _ _ _) canceler) nw =
    let
        curNodeCancelers = Api.getNodeCancelers uuid nw
        newNodeCancelers = canceler A.: curNodeCancelers
    in
        fine $ Api.storeNodeCancelers uuid newNodeCancelers nw
applyInnerAction _ (ClearNodeCancelers (Node uuid _ _ _ _ _)) nw =
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


withE :: forall d c n. Either NoodleError (Network d c n) -> StepE d c n
withE e = e <#> (flip (/\) $ doNothing)


{-
performEffect -- TODO: move to a separate module
    :: forall d c n
     . Toolkit d c n -- TODO: check if it really needs toolkit
    -> (Action d c n -> Effect Unit)
    -> NoodleEffect d c n
    -> Network d c n
    -> Effect Unit
performEffect _ push (DoE effectful) nw = effectful nw
performEffect _ push (AddPatchE alias) _ = MOVED
performEffect _ push (AddNodeE patchPath nodeAlias n (NodeDef def)) _ = MOVED
performEffect toolkit push (AddNextNodeE patchPath n (NodeDef def)) nw = MOVED
performEffect _ push (ProcessWithE node processF) _ = do
    push $ Build $ ProcessWith node processF
performEffect _ push (AddLinkE outlet inlet) _ =  MOVED
performEffect _ push (SubscribeNodeProcess node) nw = do
    canceler <- Api.setupNodeProcessFlow node nw
    push $ Inner $ StoreNodeCanceler node canceler
performEffect _ push (CancelNodeSubscriptions node@(Node uuid _ _ _ _)) nw = do
    _ <- Api.cancelNodeSubscriptions uuid nw
    push $ Inner $ ClearNodeCancelers node
performEffect _ push (CancelInletSubscriptions inlet@(Inlet uuid _ _ _ )) nw = do
    _ <- Api.cancelInletSubscriptions uuid nw
    push $ Inner $ ClearInletCancelers inlet
performEffect _ push (CancelOutletSubscriptions outlet@(Outlet uuid _ _ _ )) nw = do
    _ <- Api.cancelOutletSubscriptions uuid nw
    push $ Inner $ ClearOutletCancelers outlet
performEffect _ push (CancelLinkSubscriptions link@(Link uuid _)) nw = do
    _ <- Api.cancelLinkSubscriptions uuid nw
    push $ Inner $ ClearLinkCancelers link
performEffect _ push (AddInletE nodePath inletAlias c) _ = MOVED
performEffect _ push (InformNodeOnInletUpdates inlet node) _ = do
    canceler <- Api.informNodeOnInletUpdates inlet node
    push $ Inner $ StoreInletCanceler inlet canceler
performEffect _ push (AddOutletE nodePath outletAlias c) _ = MOVED
performEffect _ push (InformNodeOnOutletUpdates outlet node) _ = do
    canceler :: Canceler <- Api.informNodeOnOutletUpdates outlet node
    push $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ push (SubscribeNodeUpdates node) _ = do
    canceler :: Canceler <-
        Api.subscribeNode node
            (const $ const $ const $ pure unit) -- FIXME: implement
            (const $ const $ const $ pure unit) -- FIXME: implement
    push $ Inner $ StoreNodeCanceler node canceler
performEffect _ push (SendToInletE inlet d) _ = MOVED
performEffect _ push (SendToOutletE outlet d) _ = MOVED
performEffect _ push (StreamToInletE inlet flow) _ = MOVED
performEffect _ push (StreamToOutletE outlet flow) _ = MOVED
performEffect _ push (SubscribeToInletE inlet handler) _ = MOVED
performEffect _ push (SubscribeToOutletE outlet handler) _ = MOVED
performEffect _ push
    (SubscribeToNodeE node inletsHandler outletsHandler) _ = MOVED
performEffect _ push (SendActionOnInletUpdatesE inlet@(Inlet _ path _ { flow })) _ = do
    let (InletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (push <<< Data <<< GotInletData inlet)
    push $ Inner $ StoreInletCanceler inlet canceler
performEffect _ push (SendActionOnOutletUpdatesE outlet@(Outlet _ path _ { flow })) _ = do
    let (OutletFlow flow') = flow
    canceler :: Canceler <- E.subscribe flow' (push <<< Data <<< GotOutletData outlet)
    push $ Inner $ StoreOutletCanceler outlet canceler
performEffect _ push (SendPeriodicallyToInletE inlet period fn) _ = MOVED
performEffect _ push (SendPeriodicallyToOutletE outlet period fn) _ = MOVED
-}


-- apply'
--     :: forall d c n
--      . Action d c n
--     -> (Action d c n -> Effect Unit)
--     -> Toolkit d c n
--     -> R.Network d c n
--     -> R.Noodle (R.Network d c n)
-- apply' Bang push _ nw =
--     Noodle.subscribeAllInlets onInletData nw
--         </> Noodle.subscribeAllOutlets onOutletData
--     where
--         onInletData inletPath d =
--             push $ GotInletData inletPath d
--         onOutletData outletPath d =
--             push $ GotOutletData outletPath d
-- apply' (AddPatch alias) push _ nw =
--     R.addPatch alias nw
--         -- FIXME: subscribe the nodes in the patch
-- apply' (AddNode patchPath alias n) push _ nw =
--     Noodle.addNode patchPath alias n nw
--         -- FIXME: `onInletData`/`onOutletData` do not receive the proper state
--         --        of the network this way (do they need it?), but they should
--         --        (pass the current network state in the Process function?)
--         </> Noodle.subscribeNode nodePath
--                 (onNodeInletData nodePath)
--                 (onNodeOutletData nodePath)
--     where
--         nodePath = P.nodeInPatch patchPath alias
--         (patchAlias /\ nodeAlias) = P.explodeNodePath nodePath
--         -- addModel = pure <<< ((/\) model)
--         onNodeInletData nodePath (inletAlias /\ _ /\ d) =
--             push $ GotInletData (P.toInlet patchAlias nodeAlias inletAlias) d
--         onNodeOutletData nodePath (outletAlias /\ _ /\ d) =
--             push $ GotOutletData (P.toOutlet patchAlias nodeAlias outletAlias) d
-- apply' (AddInlet nodePath alias c) push _ nw =
--     let
--         inletPath = P.inletInNode nodePath alias
--         onInletData d =
--             push $ GotInletData inletPath d
--     in
--         Noodle.addInlet nodePath alias c nw
--             </> Noodle.subscribeInlet inletPath (R.InletHandler onInletData)
-- apply' (AddOutlet nodePath alias c) push _ nw =
--     let
--         outletPath = P.outletInNode nodePath alias
--         onOutletData d =
--             push $ GotOutletData outletPath d
--     in
--         Noodle.addOutlet nodePath alias c nw
--             </> Noodle.subscribeOutlet outletPath (R.OutletHandler onOutletData)
-- apply' (Connect { inlet : inletPath, outlet : outletPath }) _ _ nw =
--     Noodle.connect outletPath inletPath nw
-- apply' (Disconnect { inlet : inletPath, outlet : outletPath }) _ _ nw =
--     Noodle.disconnectTop outletPath inletPath nw
-- apply' _ _ _ nw = pure nw

