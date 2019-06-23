
module Rpd.API
    ( RpdError(..)
    , addPatch
    ) where

import Debug.Trace

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
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
    | AddPatch UUID.ToPatch Path.ToPatch
    -- | PatchReady UUID.ToPatch -- (Patch d c n)
    | RequestToAddNode Path.ToPatch Path.Alias n
    | AddNode UUID.ToNode Path.ToPatch Path.Alias n (InletsAndOutletsFlows d)
    | ProcessWith UUID.ToNode (ProcessF d)
    | StoreNodeCancelers UUID.ToNode (Array Canceler)
    -- | CancelNodeSubscriptions
    -- | NodeReady UUID.ToNode -- (Node d n)


-- data RpdEffect =
--     = RequestToAddNode
--     | MakeUUID (UUID -> Msg)


update
    :: forall d c n
     . Msg d c n
    -> Network d c n
    -> Either RpdError (Network d c n)
            /\ Array (Effect (Msg d c n))
update NoOp nw = pure nw /\ []
update (RequestToAddPatch alias) nw =
    pure nw /\
        [ do
           uuid <- UUID.new
           pure $ AddPatch (UUID.ToPatch uuid) $ Path.toPatch alias
        ]
update (AddPatch uuid alias) nw =
    pure (addPatch uuid alias nw) /\ [ ]
update (RequestToAddNode patchPath alias n) nw =
    pure nw /\
        [ do
           uuid <- UUID.new
           flows <- makeInletOutletsFlows
           pure $ AddNode (UUID.ToNode uuid) patchPath alias n flows
        ]
update (AddNode uuid patchPatch alias n ioFlow) nw =
    addNode uuid patchPatch alias n ioFlow nw /\ [ ]
update (ProcessWith nodeUuid processF) nw =
    case saveProcessAndGetTrigger of
        Right (nw' /\ trigger) ->
            pure nw' /\
                [ trigger <#> \canceler -> StoreNodeCancelers nodeUuid [ canceler ] ]
        Left err -> Left err /\ []
    where
        saveProcessAndGetTrigger :: Either RpdError (Network d c n /\ Effect Canceler)
        saveProcessAndGetTrigger = do
            nw' <- processWith nodeUuid processF nw
            trigger <- setupNodeProcessFlow nodeUuid nw'
            pure $ nw' /\ trigger
update (StoreNodeCancelers uuid cancelers) nw =
    (pure $ storeNodeCancelers uuid cancelers nw) /\ []
    -- pure nw /\ triggerProcessFlow
    -- pure nw' /\
    --     [ do
    --         canceler <- triggerProcessFlow
    --         pure $ StoreNodeCancelers nodeUuid [ canceler ]
    --     ]


-- NB:
-- `addNode`:
--     `makeInletsOutletsFlow` before
--     `updateNodeProcessFlow` after



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
     . UUID.ToPatch
    -> Path.ToPatch
    -> Network d c n
    -> Network d c n
addPatch uuid path nw =
    let
        newPatch =
            Patch
                uuid
                path
                { nodes : Seq.empty
                , links : Seq.empty
                }
    in nw
        # setJust (_patch uuid) newPatch
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
     . UUID.ToNode
    -> Path.ToPatch
    -> Path.Alias
    -> n
    -> InletsAndOutletsFlows d
    -> Network d c n
    -> Either RpdError (Network d c n)
    -- -> Either RpdError (Network d c n /\ Node d n)
addNode uuid patchPath nodeAlias n { inlets : iFlow, outlets : oFlow } nw = do
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    let
        path = Path.nodeInPatch patchPath nodeAlias
        PushableFlow pushToInlets inletsFlow = iFlow
        PushableFlow pushToOutlets outletsFlow = oFlow
        newNode =
            Node
                uuid
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
    pure $ nw
         #  setJust (_node uuid) newNode
         #  setJust (_pathToId $ Path.lift path) (UUID.liftTagged uuid)
         #  setJust (_patchNode patchUuid uuid) unit
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


storeNodeCancelers :: forall d c n. UUID.ToNode -> Array Canceler -> Network d c n -> Network d c n
storeNodeCancelers uuid = storeCancelers $ UUID.uuid uuid


setupNodeProcessFlow
    :: forall d c n
     . UUID.ToNode
    -> Network d c n
    -> Either RpdError (Effect Canceler)
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
        -- receive = ?wh
        -- send = ?wh
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
            -- case view (_inlet $ UUID.ToInlet uuid) nw of
            --     Just (Inlet _ (InletPath nodePath' inletAlias') _) ->
                    -- TODO: check, if we really need to check the alias and path once again
                    --       may be we just may take alias and insert it into map
                --     if (nodePath' == nodePath) && (inletAlias' == inletAlias)
                --         then inletVals # Map.insert inletAlias curD
                --         else inletVals
                -- _ -> inletVals
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
