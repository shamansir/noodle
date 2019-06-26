
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
import Data.Tuple.Nested ((/\), type (/\))
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


data Msg d c n
    = NoOp
    | RequestToAddPatch Path.Alias
    | AddPatch (Patch d c n)
    | RequestToAddNode Path.ToPatch Path.Alias n
    | AddNode (Node d n)
    -- TODO: Toolkit nodes
    | ProcessWith UUID.ToNode (ProcessF d)
    | StoreNodeCanceler UUID.ToNode Canceler
    | ClearNodeCancelers UUID.ToNode
    | RequestToAddInlet Path.ToNode Path.Alias c
    | AddInlet (Inlet d c)
    | StoreInletCanceler UUID.ToInlet Canceler


data RpdEffect d c n
    = AddPatchE Path.Alias
    | AddNodeE Path.ToPatch Path.Alias n
    | AddInletE Path.ToNode Path.Alias c
    -- FIXME: make separate Effect for every case
    | SubscribeNodeProcess UUID.ToNode Subscriber
    | InformNodeOnInletUpdates UUID.ToNode UUID.ToInlet Subscriber
    | CancelNodeSubscriptions UUID.ToNode


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
update (ProcessWith nodeUuid processF) nw = do
    nw' <- processWith nodeUuid processF nw
    trigger <- setupNodeProcessFlow nodeUuid nw'
    pure $ nw' /\ [ SubscribeNodeProcess nodeUuid trigger ]
update (StoreNodeCanceler uuid canceler) nw =
    let
        curNodeCancelers = getNodeCancelers uuid nw
        newCancelers = canceler : curNodeCancelers
    in
        pure $ storeNodeCancelers uuid newCancelers nw /\ []
update (ClearNodeCancelers uuid) nw =
    pure $ clearNodeCancelers uuid nw /\ []
update (RequestToAddInlet nodePath alias c) nw =
    pure $ nw /\ [ AddInletE nodePath alias c ]
update (AddInlet inlet@(Inlet uuid path _ _)) nw = do
    nw' <- addInlet inlet nw
    nodePath <- (Path.getNodePath $ Path.lift path) # note (RpdError "")
    nodeUuid <- uuidByPath UUID.toNode nodePath nw
    informParentTrigger <- informInletParentNodeOnData uuid nodeUuid nw'
    processTrigger <- setupNodeProcessFlow nodeUuid nw'
    let nw'' = clearNodeCancelers nodeUuid nw'
    pure $ nw'' /\
        [ CancelNodeSubscriptions nodeUuid
        , SubscribeNodeProcess nodeUuid processTrigger
        , InformNodeOnInletUpdates nodeUuid uuid informParentTrigger
        -- TODO: subscribe for data updates
        ]
update (StoreInletCanceler uuid canceler) nw =
    let
        curInletCancelers = getInletCancelers uuid nw
        newCancelers = canceler : curInletCancelers
    in
        pure $ storeInletCancelers uuid newCancelers nw /\ []


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
performEffect (SubscribeNodeProcess uuid sub) pushMsg _ = do
    canceler <- sub
    pushMsg $ StoreNodeCanceler uuid canceler
performEffect (InformNodeOnInletUpdates _ uuid sub) pushMsg _ = do
    canceler <- sub
    pushMsg $ StoreInletCanceler uuid canceler
performEffect (CancelNodeSubscriptions uuid) pushMsg nw = do
    _ <- cancelNodeSubscriptions uuid nw
    pushMsg $ ClearNodeCancelers uuid



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
    :: forall d c n
     . UUID.ToNode
    -> ProcessF d
    -> Network d c n
    -> Either RpdError (Network d c n)
processWith uuid processF nw = do
    -- uuid <- nw # uuidByPath UUID.toNode path
    (Node _ path n _ state) :: Node d n <-
        view (_node uuid) nw # note (RpdError "")
    let
        newNode =
            Node
                uuid
                path
                n
                processF
                state
    pure $ nw
        # setJust (_node uuid) newNode


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


getNodeCancelers :: forall d c n. UUID.ToNode -> Network d c n -> Array Canceler
getNodeCancelers = getCancelers <<< UUID.uuid
getInletCancelers :: forall d c n. UUID.ToInlet -> Network d c n -> Array Canceler
getInletCancelers = getCancelers <<< UUID.uuid


clearNodeCancelers :: forall d c n. UUID.ToNode -> Network d c n -> Network d c n
clearNodeCancelers = clearCancelers <<< UUID.uuid


cancelNodeSubscriptions :: forall d c n. UUID.ToNode -> Network d c n -> Effect Unit
cancelNodeSubscriptions = cancelSubscriptions <<< UUID.uuid


-- TODO:
-- addOutlet
-- connect, disconnect
-- sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
-- add toolkit node, toolkit inlet/outlet


setupNodeProcessFlow
    :: forall d c n
     . UUID.ToNode
    -> Network d c n
    -> Either RpdError Subscriber
setupNodeProcessFlow uuid nw = do
    (Node _ _ _ process { inletsFlow, inlets, outlets }) <-
        view (_node uuid) nw # note (RpdError "")
    pure $
        if (Seq.null inlets || Seq.null outlets)
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
    -- TODO
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
    -- uuid <- liftEffect UUID.new
    -- PushableFlow pushToInlet inletFlow <- liftEffect makePushableFlow
    -- userCancelers :: Array Canceler <-
    --     liftEffect $ traverse (E.subscribe dataFlow) subs
    pure $ nw
        # setJust (_inlet uuid) inlet
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_nodeInlet nodeUuid uuid) unit
    --    # setJust (_cancelers uuid) [ canceler ]
    --    # updateNodeProcessFlow nodeUuid


informInletParentNodeOnData
    :: forall d c n
     . UUID.ToInlet
    -> UUID.ToNode
    -> Network d c n
    -> Either RpdError Subscriber
informInletParentNodeOnData inletUuid nodeUuid nw = do
    (Inlet _ path _ { flow }) :: Inlet d c
        <- view (_inlet inletUuid) nw
            # note (RpdError "")
    (Node _ _ _ _ { pushToInlets }) :: Node d n
        <- view (_node nodeUuid) nw
            # note (RpdError "")
    let
        (PushToInlets informNode) = pushToInlets
        (InletFlow inletFlow) = flow
    pure $ E.subscribe inletFlow (\d -> informNode (path /\ inletUuid /\ d))
