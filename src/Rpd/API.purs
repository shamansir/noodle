
module Rpd.API
    ( RpdError(..)
    , addPatch
    ) where

import Debug.Trace

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Array ((:))
import Data.List (List)
import Data.List (fromFoldable, catMaybes) as List
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..), note)
import Data.Lens (view, set, setJust)
import Data.Tuple.Nested ((/\), type (/\), over1)
import Data.Foldable (foldr)
import Data.Traversable (traverse, traverse_)

import Effect (Effect)
import Effect.Class (liftEffect)

import FRP.Event as E

import Rpd.Util (type (/->), PushableFlow(..), Subscriber, Canceler, Flow, never)
import Rpd.Path as Path
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Network
import Rpd.Optics
import Rpd.Process (InletAlias, InletHandler(..), OutletAlias, OutletHandler(..), ProcessF(..))
import Rpd.Toolkit (Toolkit(..))
import Rpd.Toolkit as Toolkit


-- type Rpd d e = ContT ...

newtype RpdError = RpdError String

instance showRpdError :: Show RpdError where show (RpdError err) = err
derive instance eqRpdError :: Eq RpdError


-- infixl 1 andThen as </>
-- other options: └, ~>, ...

type Step d c n = Either RpdError (Network d c n /\ Array (RpdEffect d c n))


-- andThen :: forall a b. Rpd a -> (a -> Rpd b) -> Rpd b
-- andThen = (>>=)

-- TODO:
-- myOp :: forall d c n.
--     Step d c n ->
--     (Network d c n -> Step d c n) ->
--     Effect (Step d c n)
-- myOp v f =
--     pure $ v >>= \(nw /\ effects) -> f nw
    -- v >>= \(nw /\ effects) ->
    --     pure $ do
    --         _ <- traverse_ identity effects
    --         pure $ f nw


data Msg d c n
    = NoOp
    | RequestToAddPatch Path.Alias
    | AddPatch (Patch d c n)
    | RequestToAddNode Path.ToPatch Path.Alias n
    | AddNode (Node d n)
    -- TODO: Toolkit nodes
    | ProcessWith (Node d n) (ProcessF d)
    | StoreNodeCanceler (Node d n) Canceler
    | ClearNodeCancelers (Node d n)
    | RequestToAddInlet Path.ToNode Path.Alias c
    | AddInlet (Inlet d c)
    | StoreInletCanceler (Inlet d c) Canceler
    | RequestToAddOutlet Path.ToNode Path.Alias c
    | AddOutlet (Outlet d c)
    | StoreOutletCanceler (Outlet d c) Canceler
    | RequestToConnect Path.ToOutlet Path.ToInlet
    | Connect (Outlet d c) (Inlet d c) Link
    | StoreLinkCanceler Link Canceler
    | ReceiveInletData (Inlet d c) d -- TODO: implement and use
    | ReceiveOutletData (Outlet d c) d -- TODO: implement and use


data RpdEffect d c n
    = AddPatchE Path.Alias
    | AddNodeE Path.ToPatch Path.Alias n
    | AddInletE Path.ToNode Path.Alias c
    | AddOutletE Path.ToNode Path.Alias c
    | AddLinkE (Outlet d c) (Inlet d c)
    | SubscribeNodeProcess (Node d n)
    | InformNodeOnInletUpdates (Inlet d c) (Node d n)
    | InformNodeOnOutletUpdates (Outlet d c) (Node d n)
    | CancelNodeSubscriptions (Node d n)
    | SubscribeNodeUpdates (Node d n)


update
    :: forall d c n
     . Msg d c n
    -> Network d c n
    -> Either RpdError (Network d c n /\ Array (RpdEffect d c n))
update NoOp nw = pure $ nw /\ []
update (RequestToAddPatch alias) nw =
    pure $ nw /\ [ AddPatchE alias ]
update (AddPatch p) nw =
    pure $ addPatch p nw /\ [ ]
update (RequestToAddNode patchPath alias n) nw =
    pure $ nw /\ [ AddNodeE patchPath alias n ]
update (AddNode node) nw = do
    nw' <- addNode node nw
    pure $ nw' /\ [ ]
update (ProcessWith node@(Node uuid _ _ _ _) processF) nw = do
    let newNode = processWith processF node
        nw' = nw # setJust (_node uuid) newNode
    pure $ nw' /\ [ SubscribeNodeProcess newNode ]
update (StoreNodeCanceler (Node uuid _ _ _ _) canceler) nw =
    let
        curNodeCancelers = getNodeCancelers uuid nw
        newCancelers = canceler : curNodeCancelers
    in
        pure $ storeNodeCancelers uuid newCancelers nw /\ []
update (ClearNodeCancelers (Node uuid _ _ _ _)) nw =
    pure $ clearNodeCancelers uuid nw /\ []
update (RequestToAddInlet nodePath alias c) nw =
    pure $ nw /\ [ AddInletE nodePath alias c ]
update (AddInlet inlet@(Inlet uuid path _ _)) nw = do
    nw' <- addInlet inlet nw
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- uuidByPath UUID.toNode nodePath nw'
    node <- view (_node nodeUuid) nw # note (RpdError "")
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnInletUpdates inlet node
        , SubscribeNodeUpdates node
        ]
update (StoreInletCanceler (Inlet uuid _ _ _) canceler) nw =
    let
        curInletCancelers = getInletCancelers uuid nw
        newCancelers = canceler : curInletCancelers
    in
        pure $ storeInletCancelers uuid newCancelers nw /\ []
update (RequestToAddOutlet nodePath alias c) nw =
    pure $ nw /\ [ AddOutletE nodePath alias c ]
update (AddOutlet outlet@(Outlet uuid path _ _)) nw = do
    nw' <- addOutlet outlet nw
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- uuidByPath UUID.toNode nodePath nw'
    node <- view (_node nodeUuid) nw # note (RpdError "")
    pure $ nw' /\
        [ CancelNodeSubscriptions node
        , SubscribeNodeProcess node
        , InformNodeOnOutletUpdates outlet node
        , SubscribeNodeUpdates node
        ]
update (StoreOutletCanceler (Outlet uuid _ _ _) canceler) nw =
    let
        curOutletCancelers = getOutletCancelers uuid nw
        newCancelers = canceler : curOutletCancelers
    in
        pure $ storeOutletCancelers uuid newCancelers nw /\ []
update (StoreLinkCanceler (Link uuid _) canceler) nw =
    let
        curLinkCancelers = getLinkCancelers uuid nw
        newCancelers = canceler : curLinkCancelers
    in
        pure $ storeLinkCancelers uuid newCancelers nw /\ []
update (RequestToConnect outletPath inletPath) nw = do
    outletUuid <- uuidByPath UUID.toOutlet outletPath nw
    inletUuid <- uuidByPath UUID.toInlet inletPath nw
    outlet <- view (_outlet outletUuid) nw # note (RpdError "")
    inlet <- view (_inlet inletUuid) nw # note (RpdError "")
    pure $ nw /\ [ AddLinkE outlet inlet ]
update (Connect _ _ link) nw = do
    nw' <- addLink link nw
    pure $ nw' /\ []
update (ReceiveInletData _ _) nw =
    pure $ nw /\ []
update (ReceiveOutletData _ _) nw =
    pure $ nw /\ []


performEffect
    :: forall d c n
     . RpdEffect d c n
    -> (Msg d c n -> Effect Unit)
    -> Network d c n
    -> Effect Unit
performEffect (AddPatchE alias) pushMsg _ = do
    uuid <- UUID.new
    let path = Path.toPatch alias
    pushMsg $ AddPatch $
        Patch
            (UUID.ToPatch uuid)
            path
            { nodes : Seq.empty
            , links : Seq.empty
            }
performEffect (AddNodeE patchPath nodeAlias n) pushMsg _ = do
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
                { inlets : Seq.empty
                , outlets : Seq.empty
                , inletsFlow : InletsFlow inletsFlow
                , outletsFlow : OutletsFlow outletsFlow
                , pushToInlets : PushToInlets pushToInlets
                , pushToOutlets : PushToOutlets pushToOutlets
                }
    pushMsg $ AddNode newNode
performEffect (AddLinkE outlet inlet) pushMsg nw = do
    uuid <- UUID.new
    let
        (Outlet ouuid _ _ { flow : outletFlow' }) = outlet
        (Inlet iuuid _ _ { push : pushToInlet' }) = inlet
        (OutletFlow outletFlow) = outletFlow'
        (PushToInlet pushToInlet) = pushToInlet'
        newLink = Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid }
    canceler :: Canceler <- E.subscribe outletFlow pushToInlet
    pushMsg $ Connect outlet inlet newLink
    pushMsg $ StoreLinkCanceler newLink canceler
performEffect (SubscribeNodeProcess node) pushMsg nw = do
    canceler <- setupNodeProcessFlow node nw
    pushMsg $ StoreNodeCanceler node canceler
performEffect (CancelNodeSubscriptions node@(Node uuid _ _ _ _)) pushMsg nw = do
    _ <- cancelNodeSubscriptions uuid nw
    pushMsg $ ClearNodeCancelers node
performEffect (AddInletE nodePath inletAlias c) pushMsg _ = do
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
    pushMsg $ AddInlet newInlet
    -- FIXME: pushMsg $ CancelNodeSubscriptions
performEffect (InformNodeOnInletUpdates inlet node) pushMsg _ = do
    canceler <- informNodeOnInletUpdates inlet node
    pushMsg $ StoreInletCanceler inlet canceler
performEffect (AddOutletE nodePath outletAlias c) pushMsg _ = do
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
    pushMsg $ AddOutlet newOutlet
    -- FIXME: pushMsg $ CancelNodeSubscriptions
performEffect (InformNodeOnOutletUpdates outlet node) pushMsg _ = do
    canceler <- informNodeOnOutletUpdates outlet node
    pushMsg $ StoreOutletCanceler outlet canceler
performEffect (SubscribeNodeUpdates node) pushMsg _ = do
    canceler <- subscribeNode node (const $ pure unit) (const $ pure unit)
    pushMsg $ StoreNodeCanceler node canceler


makePushableFlow :: forall d. Effect (PushableFlow d)
makePushableFlow = do
    { push, event } <- E.create
    pure $ PushableFlow push event


uuidByPath
    :: forall p x d c n
     . Path.MarksPath p
    => (UUID.Tagged -> Maybe x)
    -> p
    -> Network d c n
    -> Either RpdError x
uuidByPath f path nw = do
    (uuid' :: UUID.Tagged) <- view (_pathToId $ Path.lift path) nw # note (RpdError "")
    (uuid :: x) <- f uuid' # note (RpdError "")
    pure uuid


addPatch
    :: forall d c n
     . Patch d c n
    -> Network d c n
    -> Network d c n
addPatch patch@(Patch uuid path _) nw =
    nw
        # setJust (_patch uuid) patch
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_networkPatch uuid) unit


type InletsAndOutletsFlows d =
    { inlets :: PushableFlow (Path.ToInlet /\ UUID.ToInlet /\ d)
    , outlets :: PushableFlow (Path.ToOutlet /\ UUID.ToOutlet /\ d)
    }


makeInletOutletsFlows
    :: forall d
     . Effect (InletsAndOutletsFlows d)
makeInletOutletsFlows = do
    inlets <- makePushableFlow
    outlets <- makePushableFlow
    pure { inlets, outlets }


addNode
    :: forall d c n
     . Node d n
    -> Network d c n
    -> Either RpdError (Network d c n)
    -- -> Either RpdError (Network d c n /\ Node d n)
addNode node@(Node uuid path _ _ _) nw = do
    let patchPath = Path.getPatchPath $ Path.lift path
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
         # setJust (_node uuid) node
         # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
         # setJust (_patchNode patchUuid uuid) unit
        --  #  addInlets nodePath def.inletDefs
        -- </> addOutlets nodePath def.outletDefs
        -- # updateNodeProcessFlow (UUID.ToNode uuid)


-- addToolkitNode
--     :: forall d c n
--      . Toolkit.Channels d c
--     => Path.ToPatch
--     -> Path.Alias
--     -> Toolkit d c n
--     -> n
--     -> Network d c n
--     -> Network d c n
-- addToolkitNode patchPath nodeAlias (Toolkit name toolkitF) n nw = do
    -- FIXME: may be it should be default, so we always require toolkit?
    --        since it may confuse the user that when she has toolkit defined
    --        somewhere then adding the node of the type is not enough
    -- ... Or the Toolkit should always be the part of the Network --> Then remove this function
    -- nw # addDefNode patchPath nodeAlias (toolkitF n) n


-- addDefNode
--     :: forall d c n
--      . Toolkit.Channels d c
--     => Path.ToPatch
--     -> Path.Alias
--     -> Toolkit.NodeDef d c
--     -> n
--     -> Network d c n
--     -> Network d c n
-- addDefNode patchPath nodeAlias (Toolkit.NodeDef nodeDef) n nw = do
--     nw
--          #  addNode patchPath nodeAlias n
--         </> addInlets nodeDef.inlets
--         </> addOutlets nodeDef.outlets
--         </> processWith path nodeDef.process
--     where
--         path = Path.nodeInPatch patchPath nodeAlias
--         Path.ToPatch patchAlias = patchPath
--         addInlets inlets nw
--             = foldr addInlet' (pure nw) inlets
--         addOutlets outlets nw
--             = foldr addOutlet' (pure nw) outlets
--         addInlet' (Toolkit.InletAlias inletAlias /\ channel) rpd =
--             rpd </>
--                 addInlet path inletAlias channel
--         addOutlet' (Toolkit.OutletAlias outletAlias /\ channel) rpd =
--             rpd </>
--                 addOutlet path outletAlias channel


processWith
    :: forall d n
     . ProcessF d
    -> Node d n
    -> Node d n
processWith processF node =
    -- uuid <- nw # uuidByPath UUID.toNode path
    let (Node uuid path n _ state) = node
    in Node
        uuid
        path
        n
        processF
        state


storeCancelers :: forall d c n. UUID -> Array Canceler -> Network d c n -> Network d c n
storeCancelers uuid = setJust (_cancelers uuid)


getCancelers :: forall d c n. UUID -> Network d c n -> Array Canceler
getCancelers uuid nw = nw # view (_cancelers uuid) # fromMaybe []


clearCancelers :: forall d c n. UUID -> Network d c n -> Network d c n
clearCancelers uuid nw = nw # set (_cancelers uuid) Nothing


cancelSubscriptions :: forall d c n. UUID -> Network d c n -> Effect Unit
cancelSubscriptions uuid nw =
    traverse_ identity $ getCancelers uuid nw


storeNodeCancelers :: forall d c n. UUID.ToNode -> Array Canceler -> Network d c n -> Network d c n
storeNodeCancelers = storeCancelers <<< UUID.uuid
storeInletCancelers :: forall d c n. UUID.ToInlet -> Array Canceler -> Network d c n -> Network d c n
storeInletCancelers = storeCancelers <<< UUID.uuid
storeOutletCancelers :: forall d c n. UUID.ToOutlet -> Array Canceler -> Network d c n -> Network d c n
storeOutletCancelers = storeCancelers <<< UUID.uuid
storeLinkCancelers :: forall d c n. UUID.ToLink -> Array Canceler -> Network d c n -> Network d c n
storeLinkCancelers = storeCancelers <<< UUID.uuid


getNodeCancelers :: forall d c n. UUID.ToNode -> Network d c n -> Array Canceler
getNodeCancelers = getCancelers <<< UUID.uuid
getInletCancelers :: forall d c n. UUID.ToInlet -> Network d c n -> Array Canceler
getInletCancelers = getCancelers <<< UUID.uuid
getOutletCancelers :: forall d c n. UUID.ToOutlet -> Network d c n -> Array Canceler
getOutletCancelers = getCancelers <<< UUID.uuid
getLinkCancelers :: forall d c n. UUID.ToLink -> Network d c n -> Array Canceler
getLinkCancelers = getCancelers <<< UUID.uuid


clearNodeCancelers :: forall d c n. UUID.ToNode -> Network d c n -> Network d c n
clearNodeCancelers = clearCancelers <<< UUID.uuid


cancelNodeSubscriptions :: forall d c n. UUID.ToNode -> Network d c n -> Effect Unit
cancelNodeSubscriptions = cancelSubscriptions <<< UUID.uuid


-- TODO:
-- connect, disconnect
-- sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
-- add toolkit node, toolkit inlet/outlet


setupNodeProcessFlow
    :: forall d c n
     . Node d n
    -> Network d c n -- FIXME: get rid of the network usage here as well?
    -> Subscriber
setupNodeProcessFlow node nw =
    let (Node uuid _ _ process { inletsFlow, inlets, outlets }) = node
    in if (Seq.null inlets || Seq.null outlets)
        then pure $ pure unit
    else case process of
        Withhold -> pure $ pure unit
        processF -> do
            _ <- view (_cancelers $ UUID.uuid uuid) nw
                    # fromMaybe []
                    # traverse_ liftEffect
            if (Seq.null inlets || Seq.null outlets) then pure $ pure unit else do
                let
                    (outletFlows :: UUID.ToOutlet /-> PushToOutlet d) =
                        outlets
                            # (Seq.toUnfoldable :: forall a. Seq a -> List a)
                            # map (\ouuid ->
                                view (_outletPush ouuid) nw
                                    <#> \push -> ouuid /\ push)
                            # List.catMaybes -- FIXME: raise an error if outlet wasn't found
                            # Map.fromFoldable
                    pushToOutletFlow :: (Path.ToOutlet /\ UUID.ToOutlet /\ d) -> Effect Unit
                    pushToOutletFlow (_ /\ ouuid /\ d) =
                        case Map.lookup ouuid outletFlows of
                            Just (PushToOutlet push) -> push d
                            Nothing -> pure unit
                OutletsFlow outletsFlow /\ maybeCancelBuild <-
                    buildOutletsFlow uuid process inletsFlow inlets outlets nw
                canceler :: Canceler
                    <- E.subscribe outletsFlow pushToOutletFlow
                pure $ case maybeCancelBuild of
                            Just buildCanceler -> canceler <> buildCanceler
                            Nothing -> canceler


buildOutletsFlow
    :: forall d c n
     . UUID.ToNode -- FIXME: we don't use UUID here
    -> ProcessF d
    -> InletsFlow d
    -> Seq UUID.ToInlet
    -> Seq UUID.ToOutlet
    -> Network d c n
    -> Effect (OutletsFlow d /\ Maybe Canceler) -- FIXME: for now, we only need Rpd to handle the
buildOutletsFlow _ Withhold _ _ _ _ =
    -- liftEffect never >>= pure <<< OutletsFlow
    never >>= \flow ->
        pure $ OutletsFlow flow /\ Nothing
-- buildOutletsFlow nodePath PassThrough inletsFlow inlets outlets nw =
--     -- collect aliases for all inlets and outlets in the node, subscribe to inlets flow
--     -- every call to `receive`
--     buildOutletsFlow
--         nodePath
--         (Process ?wh ?wh)
--         inletsFlow
--         inlets
--         outlets
--         nw
buildOutletsFlow _ (Process processNode) (InletsFlow inletsFlow) inlets outlets nw = do
    -- collect data from inletsFlow into the Map (Alias /-> d) and pass Map.lookup to the `processF` handler.
    { push, event } <- E.create
    let
        outletsAliases :: List (Path.ToOutlet /\ UUID.ToOutlet)
        outletsAliases =
            (outlets
                 # List.fromFoldable
                <#> \uuid ->
                    view (_outlet uuid) nw
                        <#> \(Outlet uuid' path _ _) -> path /\ uuid')
                 # List.catMaybes
                -- FIXME: if outlet wasn't found, raise an error?
                --  # Map.fromFoldable
        foldingF
            ((Path.ToInlet { node : nodePath, inlet : inletAlias })
            /\ uuid
            /\ curD)
            inletVals =
            inletVals # Map.insert inletAlias curD
        processFlow = E.fold foldingF inletsFlow Map.empty
        processHandler inletsValues = do
            -- TODO: could even produce Aff (and then cancel it on next iteration)
            let receive = flip Map.lookup $ inletsValues
            (send :: Path.Alias -> Maybe d) <- processNode receive
            _ <- traverse
                    (\(path@(Path.ToOutlet { outlet : alias }) /\ uuid) ->
                        case send alias of
                            Just v -> push $ path /\ uuid /\ v
                            Nothing -> pure unit
                    )
                    outletsAliases
            pure unit
    canceler <- liftEffect $ E.subscribe processFlow processHandler
    pure $ (OutletsFlow event)
           /\ Just canceler


addInlet
    :: forall d c n
     . Inlet d c
    -> Network d c n
    -> Either RpdError (Network d c n)
addInlet inlet@(Inlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # setJust (_inlet uuid) inlet
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_nodeInlet nodeUuid uuid) unit


informNodeOnInletUpdates
    :: forall d c n
     . Inlet d c
    -> Node d n
    -> Subscriber
informNodeOnInletUpdates inlet node = do
    let Inlet uuid path _ { flow } = inlet
        Node _ _ _ _ { pushToInlets } = node
        (PushToInlets informNode) = pushToInlets
        (InletFlow inletFlow) = flow
    E.subscribe inletFlow (\d -> informNode (path /\ uuid /\ d))


addOutlet
    :: forall d c n
     . Outlet d c
    -> Network d c n
    -> Either RpdError (Network d c n)
addOutlet outlet@(Outlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # setJust (_outlet uuid) outlet
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_nodeOutlet nodeUuid uuid) unit


informNodeOnOutletUpdates
    :: forall d c n
     . Outlet d c
    -> Node d n
    -> Subscriber
informNodeOnOutletUpdates outlet node = do
    let (Outlet uuid path _ { flow }) = outlet
        (Node _ _ _ _ { pushToOutlets }) = node
        (PushToOutlets informNode) = pushToOutlets
        (OutletFlow outletFlow) = flow
    E.subscribe outletFlow (\d -> informNode (path /\ uuid /\ d))


subscribeNode
    :: forall d n
     . Node d n
    -> (InletAlias /\ UUID.ToInlet /\ d -> Effect Unit) -- FIXME: change to (Inlet d c)
    -> (OutletAlias /\ UUID.ToOutlet /\ d -> Effect Unit) -- FIXME: change to (Outlet d c)
    -> Effect Canceler
subscribeNode (Node _ _ _ _ { inletsFlow, outletsFlow }) inletsHandler outletsHandler = do
    let InletsFlow inletsFlow' = inletsFlow
    let OutletsFlow outletsFlow' = outletsFlow
    inletsCanceler :: Canceler <-
        E.subscribe inletsFlow'
            (inletsHandler <<< over1 \(Path.ToInlet { inlet }) -> inlet)
    outletsCanceler :: Canceler <-
        E.subscribe outletsFlow'
            (outletsHandler <<< over1 \(Path.ToOutlet { outlet }) -> outlet)
    pure $ inletsCanceler <> outletsCanceler


addLink :: forall d c n
     . Link
    -> Network d c n
    -> Either RpdError (Network d c n)
addLink link@(Link uuid { outlet, inlet }) nw = do
    (Outlet _ opath _ _) <- view (_outlet outlet) nw # note (RpdError "")
    let patchPath = Path.getPatchPath $ Path.lift opath
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
            # setJust (_link uuid) link
            # setJust (_patchLink patchUuid uuid) unit
