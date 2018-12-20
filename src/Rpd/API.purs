
module Rpd.API
    ( Rpd, RpdError, init
    , (</>), rpdBind
    --, RpdOp, RpdEffOp
        --, emptyNetwork
    --, network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    , connect, disconnectAll --, disconnectTop
    , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    , subscribeInlet, subscribeOutlet, subscribeAllInlets, subscribeAllOutlets
    , subscribeChannelsData, subscribeNode  -- subscribeAllData
    , sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
    --, findPatch, findNode, findOutlet, findInlet
    ) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Bitraversable (bisequence)
import Data.Either (Either, note)
import Data.Foldable (foldr)
import Data.Lens (view, set, setJust)
import Data.Lens.At (at)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Control.MonadZero (empty)
import Control.Monad.Except.Trans (ExceptT, except)

import Effect (Effect, foreachE)
import Effect.Class (liftEffect)

import FRP.Event (filterMap)
import FRP.Event as E


import Rpd.Path
import Rpd.Def
import Rpd.Optics
import Rpd.Network
import Rpd.Network (empty) as Network
import Rpd.Util (type (/->), PushableFlow(..), Subscriber, Canceler, Flow)
import Rpd.Util as RU


--import Rpd.Flow as Flow

-- data RunningNetwork d e = RpdEff e (Network d e)

data RpdError = RpdError String


type RpdOp a = Either RpdError a
-- TODO: MonadEffect + MonadThrow
--       https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
type Rpd a = ExceptT RpdError Effect a
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


infixl 1 rpdBind as </>
-- other options: └, ~>, ...


rpdBind :: forall a b. Rpd a -> (a -> Rpd b) -> Rpd b
rpdBind = (>>=)


someApiFunc :: forall d. Rpd (Network d)
someApiFunc =
    init "t"
        </> addPatch "foo"
        </> addNode (PatchId 0) "test1"
        </> addNode (PatchId 0) "test2"


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


init :: forall d. String -> Rpd (Network d)
init = pure <<< Network.empty


makePushableFlow :: forall d. Effect (PushableFlow d)
makePushableFlow = do
    { push, event } <- E.create
    pure $ PushableFlow push event


nextPatchId :: forall d. Network d -> PatchId
nextPatchId (Network _ { patches }) =
    PatchId (Map.size patches)


nextNodePath :: forall d. PatchId -> Network d -> Either RpdError NodePath
nextNodePath patchId (Network _ { patches }) = do
    (Patch _ _ { nodes }) <- Map.lookup patchId patches
                                # note (RpdError "")
    pure $ NodePath patchId $ Set.size nodes


nextInletPath :: forall d. NodePath -> Network d -> Either RpdError InletPath
nextInletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { inlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ InletPath nodePath $ Set.size inlets


nextOutletPath :: forall d. NodePath -> Network d -> Either RpdError OutletPath
nextOutletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { outlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ OutletPath nodePath $ Set.size outlets


nextLinkId :: forall d. Network d -> LinkId
nextLinkId (Network _ { links }) =
    LinkId (Map.size links)



exceptMaybe :: forall a. RpdError -> Maybe a -> ExceptT RpdError Effect a
exceptMaybe err maybe =
    except (maybe # note err)


addPatch :: forall d. String -> Network d -> Rpd (Network d)
addPatch name =
    addPatch'
        { name
        , nodeDefs : List.Nil
        }


addPatch'
    :: forall d
     . PatchDef d
    -> Network d
    -> Rpd (Network d)
addPatch' patchDef nw =
    pure $ setJust (_patch patchId) newPatch nw
    where
        patchId = nextPatchId nw
        newPatch =
            Patch
                patchId
                patchDef
                { nodes : Set.empty
                }

-- TODO: removePatch
    -- TODO: cancel all the cancelers related to the patch

addNode
    :: forall d
     . PatchId
    -> String
    -> Network d
    -> Rpd (Network d)
addNode patchId name =
    addNode'
        patchId
        { name
        , inletDefs : List.Nil
        , outletDefs : List.Nil
        , process : withhold
        }


addNode'
    :: forall d
     . PatchId
    -> NodeDef d
    -> Network d
    -> Rpd (Network d)
addNode' patchId def nw = do
    nodePath <- except $ nextNodePath patchId nw
    dataPFlow@(PushableFlow _ dataFlow) <- liftEffect makePushableFlow
    let
        processFlow = makeProcessFlow dataFlow
        newNode =
            Node
                nodePath
                def
                { inlets : Set.empty
                , outlets : Set.empty
                , flow : dataPFlow
                , processFlow : processFlow -- must be an event doing nothing at first
                }
    nw
         #  setJust (_node nodePath) newNode
         #  setJust (_patchNode patchId nodePath) unit
         #  addInlets nodePath def.inletDefs
        </> addOutlets nodePath def.outletDefs
        </> updateNodeProcessFlow nodePath


addInlet
    :: forall d
     . NodePath
    -> String
    -> Network d
    -> Rpd (Network d)
addInlet nodePath label =
    addInlet'
        nodePath
        { label
        , default : Nothing
        , accept : Nothing
        }

addInlet'
    :: forall d
     . NodePath
    -> InletDef d
    -> Network d
    -> Rpd (Network d)
addInlet' nodePath def nw = do
    inletPath <- except $ nextInletPath nodePath nw
    -- TODO: when there's already some inlet exists with the same path,
    -- cancel its subscription before
    pushableFlow@(PushableFlow pushData dataFlow) <- liftEffect makePushableFlow
    (Node _ _ { flow }) :: Node d <- view (_node nodePath) nw # exceptMaybe (RpdError "")
    let (PushableFlow pushNodeData _ ) = flow
    let
        newInlet =
            Inlet
                inletPath
                def
                { flow : pushableFlow
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe dataFlow (\d -> pushNodeData (inletPath /\ d))
    nw # setJust (_inlet inletPath) newInlet
       # setJust (_nodeInlet nodePath inletPath) unit
       # setJust (_inletCanceler inletPath) canceler
       # updateNodeProcessFlow nodePath


addInlets :: forall d. NodePath -> List (InletDef d) -> Network d -> Rpd (Network d)
addInlets nodePath inletDefs nw =
    -- FIXME: may appear not very optimal, since every `addInlet'`
    --        call looks for the node again and again
    foldr foldingF (pure nw) inletDefs
    where
        foldingF inletDef rpd =
            rpd </> addInlet' nodePath inletDef


-- TODO: removeInlet
    -- TODO: execute the corresponding process canceler
    -- TODO: cancel all the links going into this inlet
    -- TODO: updateNodeProcessFlow


addOutlet
    :: forall d
     . NodePath
    -> String
    -> Network d
    -> Rpd (Network d)
addOutlet nodePath label =
    addOutlet'
        nodePath
        { label
        , accept : empty
        }


addOutlet'
    :: forall d
     . NodePath
    -> OutletDef d
    -> Network d
    -> Rpd (Network d)
addOutlet' nodePath def nw = do
    outletPath <- except $ nextOutletPath nodePath nw
    pushableFlow <- liftEffect makePushableFlow
    let
        newOutlet =
            Outlet
                outletPath
                def
                { flow : pushableFlow
                }
    nw # setJust (_outlet outletPath) newOutlet
       # setJust (_nodeOutlet nodePath outletPath) unit
       # updateNodeProcessFlow nodePath


addOutlets :: forall d. NodePath -> List (OutletDef d) -> Network d -> Rpd (Network d)
addOutlets nodePath outletDefs nw =
    -- FIXME: may appear not very optimal, since every `addOutlet'`
    --        call looks for the node again and again
    foldr foldingF (pure nw) outletDefs
    where
        foldingF outletDef rpd =
            rpd </> addOutlet' nodePath outletDef


-- TODO: removeOutlet
    -- TODO: cancel all the links going from this outlet
    -- TODO: updateNodeProcessFlow


sendToInlet
    :: forall d
     . InletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToInlet inletPath d nw = do
    (PushableFlow push _) <-
        view (_inletPFlow inletPath) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToInlet
    :: forall d
     . InletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToInlet inletPath flow nw = do
    (PushableFlow push _) <-
        view (_inletPFlow inletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


sendToOutlet
    :: forall d
     . OutletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToOutlet outletPath d nw = do
    (PushableFlow push _) <-
        view (_outletPFlow outletPath) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToOutlet
    :: forall d
     . OutletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToOutlet outletPath flow nw = do
    (PushableFlow push _) <-
        view (_outletPFlow outletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


subscribeInlet
    :: forall d
     . InletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeInlet inletPath handler nw = do
    flow :: Flow d <-
        view (_inletFlow inletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeOutlet
    :: forall d
     . OutletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeOutlet outletPath handler nw = do
    flow :: Flow d <-
        view (_outletFlow outletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow handler
    pure canceler


subscribeAllInlets
    :: forall d
     . (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (InletPath /-> Canceler)
subscribeAllInlets handler (Network _ { inlets }) =
    traverse sub inlets
    where
        sub :: Inlet d -> Subscriber
        sub (Inlet inletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> E.subscribe fl $ handler inletPath


subscribeAllOutlets
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (OutletPath /-> Canceler)
subscribeAllOutlets handler (Network _ { outlets }) =
    traverse sub outlets
    where
        sub :: Outlet d -> Subscriber
        sub (Outlet outletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> E.subscribe fl $ handler outletPath


subscribeChannelsData
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect ((OutletPath /-> Canceler) /\ (InletPath /-> Canceler))
subscribeChannelsData oHandler iHandler nw =
    bisequence $ subscribeAllOutlets oHandler nw /\ subscribeAllInlets iHandler nw


subscribeNode
    :: forall d
     . NodePath
    -> (InletPath /\ d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeNode nodePath handler nw = do
    flow :: Flow (InletPath /\ d) <-
        view (_nodeFlow nodePath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow handler
    pure canceler


connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
-- FIXME: rewrite for the case of different patches
connect outletPath inletPath
    nw@(Network nwdef nwstate) = do
    let
        linkId = nextLinkId nw
        newLink = Link outletPath inletPath
        iNodePath = getNodeOfInlet inletPath
        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

    outletPFlow <- view (_outletPFlow outletPath) nw # exceptMaybe (RpdError "")
    inletPFlow <- view (_inletPFlow inletPath) nw # exceptMaybe (RpdError "")

    let
        (PushableFlow _ outletFlow) = outletPFlow
        (PushableFlow pushToInlet inletFlow) = inletPFlow

    linkCanceler :: Canceler <-
            liftEffect $
                E.subscribe outletFlow pushToInlet

    pure $ nw
            # setJust (_link linkId) newLink
            # setJust (_linkCanceler linkId) linkCanceler


disconnectAll
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
disconnectAll outletPath inletPath
    nw@(Network nwdef nwstate@{ links }) = do
    let
        linkForDeletion (Link outletPath' inletPath') =
            (outletPath' == outletPath) && (inletPath' == inletPath)
        linksForDeletion = Map.keys $ links # Map.filter linkForDeletion

        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

    _ <- liftEffect $ traverse_
            (\linkId -> fromMaybe (pure unit) $ view (_linkCanceler linkId) nw)
            linksForDeletion

    pure $ (
        foldr (\linkId nw ->
            nw # set (_link linkId) Nothing
               # set (_linkCanceler linkId) Nothing
        ) nw linksForDeletion
        -- # setJust (_inletConnections inletPath) newInletConnections
        -- # setJust (_outletConnections outletPath) newOutletConnections
    )

    -- TODO: un-subscribe `process`` function of the target node to update values including this connection

-- TODO: disconnectTop

-- TODO: disconnectTopOf (OutletPath /\ InletPath)

-- TODO: subscribeAllNodes

-- TODO: subscribeAllData

updateNodeProcessFlow
    :: forall d
     . NodePath
    -> Network d
    -> Rpd (Network d)
updateNodeProcessFlow nodePath nw = do
    -- cancel the previous subscription if it exists
    _ <- view (_nodeCanceler nodePath) nw
            # fromMaybe (pure unit)
            # liftEffect
    (Node _ nodeDef { processFlow, inlets, outlets }) <-
        except $ view (_node nodePath) nw # note (RpdError "")
    if (isJust nodeDef.process
        || Set.isEmpty inlets
        || Set.isEmpty outlets) then pure nw else do
        (PushableFlow _ dataFlow) <-
            except $ view (_nodePFlow nodePath) nw # note (RpdError "")
        let
            (processHandler
                :: (InletPath /-> d)
                -> RU.PushF (OutletPath /-> d)
                -> Effect Unit) =
                nw # ?wh nodeDef.process outlets
        canceler :: Canceler
            <- liftEffect $ E.subscribe processFlow processHandler
        pure $ nw # setJust (_nodeCanceler nodePath) canceler
    -- where
    --     noProcessNeeded FlowThrough = true
    --     noProcessNeeded _ = false



-- findFittingProcessHandler
--     :: forall d
--      . ProcessF d
--     -> Set OutletPath
--     -> Network d
--     -> (InletPath /-> d)
--     -> Effect Unit
-- findFittingProcessHandler (LabelBased processF) outlets nw =
--     let
--         (outletLabelToFlow :: String /-> PushableFlow d) =
--             outlets
--                 # (Set.toUnfoldable :: forall a. Set a -> Array a)
--                 # map (\outletPath ->
--                     view (_outlet outletPath) nw)
--                 # filterMap (\maybeOutlet ->
--                     maybeOutlet >>= \(Outlet _ { label } { flow }) ->
--                         pure (label /\ flow))
--                 # Map.fromFoldable
--         ph = nw # makeProcessHandler processF outletLabelToFlow
--         pathToLabel inletPath = -- FIXME
--             fromMaybe "<?>" $ view (_inletLabel inletPath) nw
--     in
--         ph <<< RU.convertKeysInMap pathToLabel
-- findFittingProcessHandler (IndexBased processF) outlets nw =
--     let
--         (outletFlows :: Array (PushableFlow d)) =
--             outlets
--                 # (Set.toUnfoldable :: forall a. Set a -> Array a)
--                 # map (\outletPath -> view (_outletPFlow outletPath) nw)
--                 # filterMap identity
--         ph = nw # makeProcessHandler' processF outletFlows
--     in
--         ph <<< List.toUnfoldable <<< Map.values
-- findFittingProcessHandler FlowThrough outlets nw =
--     const $ pure unit


makeProcessFlow :: forall d. Flow (InletPath /\ d) -> Flow (InletPath /-> d)
makeProcessFlow dataFlow =
    E.fold (uncurry Map.insert) dataFlow Map.empty


makeProcessHandler
    :: forall d
     . (String /-> d -> String /-> d)
    -> (String /-> PushableFlow d)
    -> Network d
    -> (String /-> d)
    -> Effect Unit
makeProcessHandler processF outletFlows nw inletVals = do
    let (outletVals :: String /-> d) = processF inletVals
    foreachE (Set.toUnfoldable $ Map.keys outletVals) (pushToOutlet outletVals)
    pure unit
    where
        getInletId (InletPath _ inletId) = inletId
        pushToOutlet outletVals outletLabel =
            case view (at outletLabel) outletFlows of
                Just (PushableFlow push _) -> do
                    maybe
                        (pure unit)
                        (\d -> do
                            _ <- push d
                            pure unit
                        ) $ view (at outletLabel) outletVals
                Nothing -> do
                    pure unit


makeProcessHandler'
    :: forall d
     . (Array d -> Array d)
    -> Array (PushableFlow d)
    -> Network d
    -> Array d
    -> Effect Unit
makeProcessHandler' processF outletFlows nw inletVals = do
    let (outletVals :: Array d) = processF inletVals
    _ <- sequence $ Array.mapWithIndex pushToOutlet outletVals
    pure unit
    where
        pushToOutlet outletIndex d =
            case outletFlows !! outletIndex of
                Just (PushableFlow push _) -> do
                    _ <- push d
                    pure unit
                Nothing -> do
                    pure unit


-- TODO: rollback :: RpdError -> Network -> Network


instance showRpdError :: Show RpdError where
    -- show (RpdError text) = "(RpdError)" <> text
    show (RpdError text) = text



-- instance eqDataSource :: Eq (DataSource d) where
--     eq (OutletSource oa a) (OutletSource ob b) = oa == ob
--     eq _ _ = false


-- instance ordDataSource :: Ord (DataSource d) where
--     compare (OutletSource oa a) (OutletSource ob b) = compare oa ob
--     compare _ _ = LT

