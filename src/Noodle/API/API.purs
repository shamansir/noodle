
module Noodle.API where

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (either)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.List (List)
import Data.List (fromFoldable, catMaybes) as List
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..), note)
import Data.Lens (view, set, setJust)
import Data.Tuple.Nested ((/\), type (/\), over1)
import Data.Foldable (foldr)
import Data.Traversable (traverse, traverse_)
import Data.Exists (mkExists, runExists)
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Aff (runAff_)

import FRP.Event as E
import FRP.Event.Class (count) as E
import FRP.Event.Time as E

import Noodle.Util (type (/->), PushableFlow(..), Subscriber, Canceler, Flow, never)
import Noodle.Path as Path
import Noodle.UUID (UUID)
import Noodle.UUID as UUID
import Noodle.Network
import Noodle.Optics
import Noodle.Process
    ( InletAlias, InletHandler(..)
    , OutletAlias, OutletHandler(..)
    , ProcessF(..), ProcessST'(..)
    )
import Noodle.Toolkit (Toolkit(..))
import Noodle.Toolkit as Toolkit

import Noodle.API.Errors (NoodleError)
import Noodle.API.Errors as Err

-- type Noodle d e = ContT ...


-- infixl 1 andThen as </>
-- other options: └, ~>, ...


type InletSubscription d = (d -> Effect Unit)
type OutletSubscription d = (d -> Effect Unit)
type InletPeriodSubscription d = (Int -> d)
type OutletPeriodSubscription d = (Int -> d)
type NodeInletsSubscription d = (InletAlias -> UUID.ToInlet -> d -> Effect Unit) -- FIXME: change to (Inlet d c)
type NodeOutletsSubscription d = (OutletAlias -> UUID.ToOutlet -> d -> Effect Unit) -- FIXME: change to (Outlet d c)


-- andThen :: forall a b. Noodle a -> (a -> Noodle b) -> Noodle b
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
    -> Either NoodleError x
uuidByPath f path nw = do
    (uuid' :: UUID.Tagged)
        <- view (_pathToId $ Path.lift path) nw # note (Err.ftfu $ Path.lift path)
    (uuid :: x) <- f uuid' # note (Err.ftfu $ Path.lift path)
    pure uuid


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


cancelNodeSubscriptions :: forall d c n. UUID.ToNode -> Network d c n -> Effect Unit
cancelNodeSubscriptions = cancelSubscriptions <<< UUID.uuid
cancelInletSubscriptions :: forall d c n. UUID.ToInlet -> Network d c n -> Effect Unit
cancelInletSubscriptions = cancelSubscriptions <<< UUID.uuid
cancelOutletSubscriptions :: forall d c n. UUID.ToOutlet -> Network d c n -> Effect Unit
cancelOutletSubscriptions = cancelSubscriptions <<< UUID.uuid
cancelLinkSubscriptions :: forall d c n. UUID.ToLink -> Network d c n -> Effect Unit
cancelLinkSubscriptions = cancelSubscriptions <<< UUID.uuid


clearNodeCancelers :: forall d c n. UUID.ToNode -> Network d c n -> Network d c n
clearNodeCancelers = clearCancelers <<< UUID.uuid
clearInletCancelers :: forall d c n. UUID.ToInlet -> Network d c n -> Network d c n
clearInletCancelers = clearCancelers <<< UUID.uuid
clearOutletCancelers :: forall d c n. UUID.ToOutlet -> Network d c n -> Network d c n
clearOutletCancelers = clearCancelers <<< UUID.uuid
clearLinkCancelers :: forall d c n. UUID.ToLink -> Network d c n -> Network d c n
clearLinkCancelers = clearCancelers <<< UUID.uuid


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


addNode
    :: forall d c n
     . Node d n
    -> Network d c n
    -> Either NoodleError (Network d c n)
    -- -> Either NoodleError (Network d c n /\ Node d n)
addNode node@(Node uuid path _ _ _) nw = do
    let patchPath = Path.getPatchPath $ Path.lift path
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
         # setJust (_node uuid) node
         # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
         # setJust (_patchNode patchUuid uuid) unit


removeNode
    :: forall d c n
     . Node d n
    -> Network d c n
    -> Either NoodleError (Network d c n)
removeNode node@(Node uuid path _ _ _) nw = do
    let patchPath = Path.getPatchPath $ Path.lift path
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
        # set (_node uuid) Nothing
        # set (_pathToId $ Path.lift path) Nothing
        # set (_patchNode patchUuid uuid) Nothing


{-
addDefNode
    :: forall d c n
     . Toolkit.Channels d c
    => Path.ToPatch
    -> Path.Alias
    -> Toolkit.NodeDef d c
    -> n
    -> Network d c n
    -> Network d c n
addDefNode patchPath nodeAlias (Toolkit.NodeDef nodeDef) n nw = do
    nw
         #  addNode patchPath nodeAlias n
        </> addInlets nodeDef.inlets
        </> addOutlets nodeDef.outlets
        </> processWith path nodeDef.process
    where
        path = Path.nodeInPatch patchPath nodeAlias
        Path.ToPatch patchAlias = patchPath
        addInlets inlets nw
            = foldr addInlet' (pure nw) inlets
        addOutlets outlets nw
            = foldr addOutlet' (pure nw) outlets
        addInlet' (Toolkit.InletAlias inletAlias /\ channel) noodle =
            noodle </>
                addInlet path inletAlias channel
        addOutlet' (Toolkit.OutletAlias outletAlias /\ channel) noodle =
            noodle </>
                addOutlet path outletAlias channel
-}


addOutlet
    :: forall d c n
     . Outlet d c
    -> Network d c n
    -> Either NoodleError (Network d c n)
addOutlet outlet@(Outlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # setJust (_outlet uuid) outlet
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_nodeOutlet nodeUuid uuid) unit


addInlet
    :: forall d c n
     . Inlet d c
    -> Network d c n
    -> Either NoodleError (Network d c n)
addInlet inlet@(Inlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # setJust (_inlet uuid) inlet
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
        # setJust (_nodeInlet nodeUuid uuid) unit


removeInlet
    :: forall d c n
     . Inlet d c
    -> Network d c n
    -> Either NoodleError (Network d c n)
removeInlet inlet@(Inlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # set (_inlet uuid) Nothing
        # set (_pathToId $ Path.lift path) Nothing
        # set (_nodeInlet nodeUuid uuid) Nothing


removeOutlet
    :: forall d c n
     . Outlet d c
    -> Network d c n
    -> Either NoodleError (Network d c n)
removeOutlet outlet@(Outlet uuid path _ _) nw = do
    nodePath <- (Path.getNodePath $ Path.lift path) # note (Err.nnp $ Path.lift path)
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    pure $ nw
        # set (_outlet uuid) Nothing
        # set (_pathToId $ Path.lift path) Nothing
        # set (_nodeOutlet nodeUuid uuid) Nothing


addLink :: forall d c n
     . Link
    -> Network d c n
    -> Either NoodleError (Network d c n)
addLink link@(Link uuid { outlet, inlet }) nw = do
    (Outlet _ opath _ _) <- view (_outlet outlet) nw # note (Err.ftfs $ UUID.uuid outlet)
    let patchPath = Path.getPatchPath $ Path.lift opath
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
            # setJust (_link uuid) link
            # setJust (_patchLink patchUuid uuid) unit


removeLink :: forall d c n
     . Link
    -> Network d c n
    -> Either NoodleError (Network d c n)
removeLink link@(Link uuid { outlet, inlet }) nw = do
    (Outlet _ opath _ _) <- view (_outlet outlet) nw # note (Err.ftfs $ UUID.uuid outlet)
    let patchPath = Path.getPatchPath $ Path.lift opath
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    pure $ nw
        # set (_link uuid) Nothing
        # set (_patchLink patchUuid uuid) Nothing


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



setupNodeProcessFlow
    :: forall d c n
     . Node d n
    -> Network d c n -- FIXME: get rid of the network usage here as well?
    -> Subscriber
setupNodeProcessFlow node nw =
    let (Node uuid _ _ process { inletsFlow, inlets, outlets }) = node
    in if (Seq.null inlets)
        then pure $ pure unit
    else case process of
        Withhold -> pure $ pure unit
        processF -> do
            _ <- view (_cancelers $ UUID.uuid uuid) nw
                    # fromMaybe []
                    # traverse_ liftEffect
            if (Seq.null inlets) then pure $ pure unit else do
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
    -> Effect (OutletsFlow d /\ Maybe Canceler) -- FIXME: for now, we only need Noodle to handle the
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
buildOutletsFlow _ (ProcessAff processNode) (InletsFlow inletsFlow) inlets outlets nw = do
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
        processHandler :: _ -> Effect Unit
        processHandler inletsValues = do
            let receive = flip Map.lookup $ inletsValues
            _ <-
                runAff_
                (either
                    (const $ pure unit)
                    $ \(send :: Path.Alias -> Maybe d) -> do
                        _ <- traverse
                            (\(path@(Path.ToOutlet { outlet : alias }) /\ uuid) ->
                                case send alias of
                                    Just v -> push $ path /\ uuid /\ v
                                    Nothing -> pure unit
                            )
                            outletsAliases
                        pure unit
                )
                $ processNode receive
            pure unit
    canceler <- liftEffect $ E.subscribe processFlow processHandler
    pure $ (OutletsFlow event)
           /\ Just canceler
buildOutletsFlow _ (ProcessST processSt) (InletsFlow inletsFlow) inlets outlets nw = do
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

    processHandler <- runExists
        (\(ProcessST' (initialState /\ processNode)) -> do
            stateRef <- Ref.new initialState
            pure (\inletsValues -> do
                let receive = flip Map.lookup $ inletsValues
                curState <- Ref.read stateRef
                nextState /\ send <- processNode (curState /\ receive)
                _ <- Ref.write nextState stateRef
                _ <- traverse
                        (\(path@(Path.ToOutlet { outlet : alias }) /\ uuid) ->
                            case send alias of
                                Just v -> push $ path /\ uuid /\ v
                                Nothing -> pure unit
                        )
                        outletsAliases
                pure unit
            )
        )
        processSt
    canceler <- liftEffect $ E.subscribe processFlow processHandler
    pure $ (OutletsFlow event)
            /\ Just canceler


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
    -> NodeInletsSubscription d
    -> NodeOutletsSubscription d
    -> Effect Canceler
subscribeNode (Node _ _ _ _ { inletsFlow, outletsFlow }) inletsHandler outletsHandler = do
    let InletsFlow inletsFlow' = inletsFlow
    let OutletsFlow outletsFlow' = outletsFlow
    inletsCanceler :: Canceler <-
        E.subscribe inletsFlow' $
            (\((Path.ToInlet { inlet }) /\ uuid /\ d) -> inletsHandler inlet uuid d)
            -- (inletsHandler <<< over1 \(Path.ToInlet { inlet }) -> inlet)
    outletsCanceler :: Canceler <-
        E.subscribe outletsFlow' $
            (\((Path.ToOutlet { outlet }) /\ uuid /\ d) -> outletsHandler outlet uuid d)
            -- (outletsHandler <<< over1 \(Path.ToOutlet { outlet }) -> outlet)
    pure $ inletsCanceler <> outletsCanceler


sendToInlet
    :: forall d c
     . Inlet d c
    -> d
    -> Effect Unit
sendToInlet (Inlet _ _ _ { push }) d =
    let (PushToInlet push') = push
    in push' d


sendToOutlet
    :: forall d c
     . Outlet d c
    -> d
    -> Effect Unit
sendToOutlet (Outlet _ _ _ { push }) d =
    let (PushToOutlet push') = push
    in push' d


streamToInlet
    :: forall d c
     . Inlet d c
    -> E.Event d
    -> Effect Canceler
streamToInlet (Inlet _ _ _ { push }) flow =
    let (PushToInlet push') = push
    in E.subscribe flow push'


streamToOutlet
    :: forall d c
     . Outlet d c
    -> E.Event d
    -> Effect Canceler
streamToOutlet (Outlet _ _ _ { push }) flow =
    let (PushToOutlet push') = push
    in E.subscribe flow push'


subscribeToInlet
    :: forall d c
     . Inlet d c
    -> InletSubscription d
    -> Effect Canceler
subscribeToInlet (Inlet _ _ _ { flow }) handler =
    let (InletFlow flow') = flow
    in E.subscribe flow' handler


subscribeToOutlet
    :: forall d c
     . Outlet d c
    -> OutletSubscription d
    -> Effect Canceler
subscribeToOutlet (Outlet _ _ _ { flow }) handler =
    let (OutletFlow flow') = flow
    in E.subscribe flow' handler


sendPeriodicallyToInlet
    :: forall d c
     . Inlet d c
    -> Int
    -> InletPeriodSubscription d
    -> Effect Canceler
sendPeriodicallyToInlet (Inlet _ _ _ { push }) period fn =
    let
        (PushToInlet push') = push
        intervalEvent = E.count $ E.interval period
    in E.subscribe intervalEvent $ push' <<< fn


sendPeriodicallyToOutlet
    :: forall d c
     . Outlet d c
    -> Int
    -> OutletPeriodSubscription d
    -> Effect Canceler
sendPeriodicallyToOutlet (Outlet _ _ _ { push }) period fn =
    let
        (PushToOutlet push') = push
        intervalEvent = E.count $ E.interval period
    in E.subscribe intervalEvent $ push' <<< fn
