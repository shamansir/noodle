
module Rpd.API
    ( Rpd, RpdError, init
    , (</>), andThen
    -- , connect, disconnectAll, disconnectTop
    -- , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    -- , removeInlet
    -- , subscribeInlet, subscribeOutlet, subscribeAllInlets, subscribeAllOutlets
    -- , subscribeChannelsData, subscribeNode  -- subscribeAllData
    -- , subscribeInlet', subscribeOutlet', subscribeAllInlets', subscribeAllOutlets'
    -- , subscribeChannelsData', subscribeNode'  -- subscribeAllData'
    -- , sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
    --, findPatch, findNode, findOutlet, findInlet
    ) where

import Debug.Trace

import Prelude

import Control.Monad.Except.Trans (ExceptT, except)
import Control.MonadZero (empty)

import Data.Array ((!!), (:), snoc)
import Data.Array as Array
import Data.Bitraversable (bisequence)
import Data.Either (Either(..), note)
import Data.Foldable (foldr)
import Data.Lens (view, set, setJust)
import Data.Lens.At (at)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, sequence, traverse, traverse_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (uncurry, fst)
import Data.Tuple.Nested ((/\), type (/\), over1)

import Effect (Effect, foreachE)
import Effect.Class (liftEffect)

import FRP.Event as E

import Rpd.Network
import Rpd.Network (empty) as Network
import Rpd.UUID as UUID
import Rpd.Util (type (/->), PushableFlow(..), Subscriber, Canceler, Flow, never)
import Rpd.Util as RU
import Rpd.Optics
import Rpd.Path (Path)
import Rpd.Path as Path
import Rpd.Process


infixl 6 snoc as +>


--import Rpd.Flow as Flow

-- data RunningNetwork d e = RpdEff e (Network d e)

newtype RpdError = RpdError String


type RpdOp a = Either RpdError a
-- TODO: MonadEffect + MonadThrow
--       https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
type Rpd a = ExceptT RpdError Effect a
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


infixl 1 andThen as </>
-- other options: └, ~>, ...


andThen :: forall a b. Rpd a -> (a -> Rpd b) -> Rpd b
andThen = (>>=)


-- TODO:
-- skipAndThen :: forall a b. Rpd (_ /\ a) -> (a -> Rpd b) -> Rpd b
-- skipAndThen = (>>=)


someApiFunc :: forall d. Rpd (Network d)
someApiFunc =
    init
        </> addPatch $ Path.toPatch "foo"
        </> addNode $ Path.toNode "foo" "test1"
        </> addNode $ Path.toNode "foo" "test2"


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


init :: forall d. Rpd (Network d)
init = pure Network.empty


-- makeUuid :: forall d. Rpd UUID.UUID
-- makeUuid = liftEffect liftEffect UUID.new


makePushableFlow :: forall d. Effect (PushableFlow d)
makePushableFlow = do
    { push, event } <- E.create
    pure $ PushableFlow push event


exceptMaybe :: forall a. RpdError -> Maybe a -> ExceptT RpdError Effect a
exceptMaybe err maybe =
    except (maybe # note err)


exceptNotFail :: RpdError -> Boolean -> ExceptT RpdError Effect Unit
exceptNotFail err bool =
    if bool then except $ Right unit else Left err


addPatch :: forall d. PatchPath -> Network d -> Rpd (Network d)
addPatch path nw = do
    _ <- Path.mayLeadToPatch path
            # exceptNotFail (RpdError "")
    uuid <- liftEffect UUID.new
    let
        newPatch =
            Patch
                uuid
                path
                Set.empty
    pure $ nw
        # setJust (_patch $ UUID.ToPatch uuid) newPatch
        # setJust (_pathToId $ Path.ToPatch path) uuid


-- TODO: removePatch
    -- TODO: cancel all the cancelers related to the patch


addNode
    :: forall d
     . NodePath
    -> Network d
    -> Rpd (Network d)
addNode path nw = do
    _ <- Path.mayLeadToNode path
            # exceptNotFail (RpdError "")
    patchPath <- Path.getPatchPath path
            # exceptMaybe (RpdError "")
    patchUuid
        <- view (_pathToId patchPath) nw
            # exceptMaybe (RpdError "")
    uuid <- liftEffect UUID.new
    PushableFlow pushToInlets inletsFlow <- liftEffect makePushableFlow
    PushableFlow pushToOutlets outletsFlow <- liftEffect makePushableFlow
    let
        nodePath = NodePath patchPath alias
        newNode =
            Node
                uuid
                nodePath
                Withhold
                { inlets : Set.empty
                , outlets : Set.empty
                , inletsFlow : InletsFlow inletsFlow
                , outletsFlow : OutletsFlow outletsFlow
                , pushToInlets : PushToInlets pushToInlets
                , pushToOutlets : PushToOutlets pushToOutlets
                }
    nw
         #  setJust (_node uuid) newNode
         #  setJust (_pathToId nodePath) uuid
         #  setJust (_patchNode patchUuid uuid) unit
        --  #  addInlets nodePath def.inletDefs
        -- </> addOutlets nodePath def.outletDefs
        # updateNodeProcessFlow uuid


processWith
    :: forall d
     . NodePath
    -> ProcessF d
    -> Network d
    -> Rpd (Network d)
processWith nodePath processF nw = do
    uuid
        <- view (_pathToId $ Path.ToNode nodePath) nw
            # exceptMaybe (RpdError "")
    (Node _ path _ state) :: Node d <-
        view (_node $ UUID.ToNode uuid) nw
            # exceptMaybe (RpdError "")
    let
        newNode =
            Node
                uuid
                path
                processF
                state
    nw
        # setJust (_node $ UUID.ToNode uuid) newNode
        # updateNodeProcessFlow (UUID.ToNode uuid)


addInlet
    :: forall d
     . NodePath
    -> Alias
    -> Network d
    -> Rpd (Network d)
addInlet nodePath alias nw = do
    nodeUuid
        <- view (_pathToId $ Path.ToNode nodePath) nw
           # exceptMaybe (RpdError "")
    uuid <- liftEffect UUID.new
    PushableFlow pushToInlet inletFlow <- liftEffect makePushableFlow
    (Node _ _ _ { pushToInlets }) :: Node d
        <- view (_node $ UUID.ToNode nodeUuid) nw
            # exceptMaybe (RpdError "")
    let
        inletPath = (InletPath nodePath alias)
        (PushToInlets informNode) = pushToInlets
        newInlet =
            Inlet
                uuid
                inletPath
                { flow : InletFlow inletFlow
                , push : PushToInlet pushToInlet
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe inletFlow (\d -> informNode (inletPath /\ uuid /\ d))
    -- userCancelers :: Array Canceler <-
    --     liftEffect $ traverse (E.subscribe dataFlow) subs
    nw # setJust (_inlet $ UUID.ToInlet uuid) newInlet
       # setJust (_pathToId $ ToInlet inletPath) uuid
       # setJust (_nodeInlet (UUID.ToNode nodeUuid) (UUID.ToInlet uuid)) unit
       # setJust (_cancelers uuid) [ canceler ]
       # updateNodeProcessFlow (UUID.ToNode nodeUuid)


-- addInlets :: forall d. NodePath -> List (InletDef d) -> Network d -> Rpd (Network d)
-- addInlets nodePath inletDefs nw =
    -- FIXME: may appear not very optimal, since every `addInlet'`
    --        call looks for the node again and again
    -- foldr foldingF (pure nw) inletDefs
    -- where
    --     foldingF inletDef rpd =
    --         rpd </> addInlet' nodePath inletDef


removeInlet
    :: forall d
     . InletPath
    -> Network d
    -> Rpd (Network d)
removeInlet (InletPath inletPath) nw = do
    nodeUuid
        <- view (_pathToId $ ToNode nodePath) nw
            # exceptMaybe (RpdError "")
    inletUuid
        <- view (_pathToId $ ToNode nodePath) nw
            # exceptMaybe (RpdError "")
    -- _ <- view (_inlet inletPath) nw # exceptMaybe (RpdError "")
    -- let (InletPath nodePath inletIdx) = inletPath
    view (_cancelers inletUuid) nw
        # fromMaybe []
        # traverse_ liftEffect
    nw  #  set (_inlet $ UUID.ToInlet inletUuid) Nothing
        #  set (_pathToId $ ToInlet inletPath) Nothing
        #  set (_nodeInlet (UUID.ToNode nodeUuid) (UUID.ToInlet inletUuid)) Nothing
        #  setJust (_cancelers inletUuid) [ ]
        #  disconnectAllComingTo (Path.ToInlet inletPath)
       </> updateNodeProcessFlow (UUID.ToNode nodeUuid)


addOutlet
    :: forall d
     . NodePath
    -> Alias
    -> Network d
    -> Rpd (Network d)
addOutlet nodePath alias nw = do
    nodeUuid
        <- view (_pathToId $ ToNode nodePath) nw
           # exceptMaybe (RpdError "")
    uuid <- liftEffect UUID.new
    PushableFlow pushToOutlet outletFlow <- liftEffect makePushableFlow
    (Node _ _ _ { pushToOutlets }) :: Node d
        <- view (_node $ UUID.ToNode nodeUuid) nw # exceptMaybe (RpdError "")
    let
        outletPath = (OutletPath nodePath alias)
        (PushToOutlets informNode) = pushToOutlets
        newOutlet =
            Outlet
                uuid
                outletPath
                { flow : OutletFlow outletFlow
                , push : PushToOutlet pushToOutlet
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe outletFlow (\d -> informNode (outletPath /\ uuid /\ d))
    nw # setJust (_outlet $ UUID.ToOutlet uuid) newOutlet
       # setJust (_pathToId $ ToOutlet outletPath) uuid
       # setJust (_nodeOutlet (UUID.ToNode nodeUuid) (UUID.ToOutlet uuid)) unit
       # setJust (_cancelers uuid) [ canceler ]
       # updateNodeProcessFlow (UUID.ToNode nodeUuid)


-- addOutlets :: forall d. NodePath -> List (OutletDef d) -> Network d -> Rpd (Network d)
-- addOutlets nodePath outletDefs nw =
--     -- FIXME: may appear not very optimal, since every `addOutlet'`
--     --        call looks for the node again and again
--     foldr foldingF (pure nw) outletDefs
--     where
--         foldingF outletDef rpd =
--             rpd </> addOutlet' nodePath outletDef


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
    inletUuid <- view (_pathToId $ ToInlet inletPath) nw # exceptMaybe (RpdError "")
    (PushToInlet push) <-
        view (_inletPush $ UUID.ToInlet inletUuid) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToInlet
    :: forall d
     . InletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToInlet inletPath flow nw = do
    inletUuid <- view (_pathToId $ ToInlet inletPath) nw # exceptMaybe (RpdError "")
    (PushToInlet push) <-
        view (_inletPush $ UUID.ToInlet inletUuid) nw # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


sendToOutlet -- TODO: consider removing?
    :: forall d
     . OutletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToOutlet outletPath d nw = do
    outletUuid <- view (_pathToId $ ToOutlet outletPath) nw # exceptMaybe (RpdError "")
    (PushToOutlet push) <-
        view (_outletPush $ UUID.ToOutlet outletUuid) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToOutlet -- TODO: consider removing?
    :: forall d
     . OutletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToOutlet outletPath flow nw = do
    outletUuid <- view (_pathToId $ ToOutlet outletPath) nw # exceptMaybe (RpdError "")
    (PushToOutlet push) <-
        view (_outletPush $ UUID.ToOutlet outletUuid) nw # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


subscribeInlet
    :: forall d
     . InletPath
    -> InletHandler d
    -> Network d
    -> Rpd (Network d)
subscribeInlet inletPath (InletHandler handler) nw = do
    inletUuid
        <- view (_pathToId $ ToInlet inletPath) nw
            # exceptMaybe (RpdError "")
    (InletFlow flow) <-
        view (_inletFlow $ UUID.ToInlet inletUuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    curCancelers <-
        view (_cancelers inletUuid) nw
            # exceptMaybe (RpdError "")
    pure $
        nw # setJust (_cancelers inletUuid) (curCancelers +> canceler)


subscribeInlet'
    :: forall d
     . InletPath
    -> InletHandler d
    -> Network d
    -> Rpd Canceler
subscribeInlet' inletPath (InletHandler handler) nw = do
    inletUuid
        <- view (_pathToId $ ToInlet inletPath) nw
           # exceptMaybe (RpdError "")
    (InletFlow flow) <-
        view (_inletFlow $ UUID.ToInlet inletUuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeOutlet
    :: forall d
     . OutletPath
    -> OutletHandler d
    -> Network d
    -> Rpd (Network d)
subscribeOutlet outletPath (OutletHandler handler) nw = do
    outletUuid
        <- view (_pathToId $ ToOutlet outletPath) nw
           # exceptMaybe (RpdError "")
    (OutletFlow flow) <-
        view (_outletFlow $ UUID.ToOutlet outletUuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    curCancelers <-
        view (_cancelers outletUuid) nw
            # exceptMaybe (RpdError "")
    pure $
        nw # setJust (_cancelers outletUuid) (curCancelers +> canceler)


subscribeOutlet'
    :: forall d
     . OutletPath
    -> OutletHandler d
    -> Network d
    -> Rpd Canceler
subscribeOutlet' outletPath (OutletHandler handler) nw = do
    outletUuid
        <- view (_pathToId $ ToOutlet outletPath) nw
           # exceptMaybe (RpdError "")
    (OutletFlow flow) <-
        view (_outletFlow $ UUID.ToOutlet outletUuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeAllInlets
    :: forall d
     . (InletPath -> d -> Effect Unit)
    -> Network d
    -> Rpd (Network d)
subscribeAllInlets handler nw = do
    _ <- liftEffect $ subscribeAllInlets' handler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeAllInlets'
    :: forall d
     . (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (InletPath /-> Canceler)
subscribeAllInlets' handler nw = do
    let
        inlets :: List (Inlet d)
        inlets = view _networkInlets nw
        pathOfInlet (Inlet _ inletPath _) = inletPath
        inletsPaths :: List InletPath
        inletsPaths = pathOfInlet <$> inlets
    cancelers :: List Canceler <- traverse sub inlets
    pure $ Map.fromFoldable $ (/\) <$> inletsPaths <*> cancelers
    where
        sub :: Inlet d -> Subscriber
        sub (Inlet _ inletPath { flow }) =
            case flow of
                InletFlow inletFlow -> E.subscribe inletFlow $ handler inletPath


subscribeAllOutlets
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> Network d
    -> Rpd (Network d)
subscribeAllOutlets handler nw = do
    _ <- liftEffect $ subscribeAllOutlets' handler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeAllOutlets'
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (OutletPath /-> Canceler)
subscribeAllOutlets' handler nw = do
    let
        outlets :: List (Outlet d)
        outlets = view _networkOutlets nw
        pathOfOutlet (Outlet _ outletPath _) = outletPath
        outletsPaths :: List OutletPath
        outletsPaths = pathOfOutlet <$> outlets
    cancelers :: List Canceler  <- traverse sub outlets
    pure $ Map.fromFoldable $ (/\) <$> outletsPaths <*> cancelers
    where
        sub :: Outlet d -> Subscriber
        sub (Outlet _ outletPath { flow }) =
            case flow of
                OutletFlow outletFlow -> E.subscribe outletFlow $ handler outletPath


subscribeChannelsData
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Rpd (Network d)
subscribeChannelsData oHandler iHandler nw = do
    _ <- liftEffect $ subscribeChannelsData' oHandler iHandler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeChannelsData'
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect ((OutletPath /-> Canceler) /\ (InletPath /-> Canceler))
subscribeChannelsData' oHandler iHandler nw =
    bisequence $ subscribeAllOutlets' oHandler nw /\ subscribeAllInlets' iHandler nw


subscribeNode
    :: forall d
     . NodePath
    -> (InletAlias /\ UUID.UUID /\ d -> Effect Unit)
    -> (OutletAlias /\ UUID.UUID /\ d -> Effect Unit)
    -> Network d
    -> Rpd (Network d)
subscribeNode nodePath inletsHandler outletsHandler nw = do
    _ <- subscribeNode' nodePath inletsHandler outletsHandler nw
    -- FIXME: implement !!!!
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeNode'
    :: forall d
     . NodePath
    -> (InletAlias /\ UUID.UUID /\ d -> Effect Unit)
    -> (OutletAlias /\ UUID.UUID /\ d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeNode' nodePath inletsHandler outletsHandler nw = do
    nodeUuid <-
        view (_pathToId $ ToNode nodePath) nw
            # exceptMaybe (RpdError "")
    InletsFlow inletsFlow <-
        view (_nodeInletsFlow $ UUID.ToNode nodeUuid) nw
            # exceptMaybe (RpdError "")
    inletsCanceler :: Canceler <-
        liftEffect $ E.subscribe inletsFlow
            (inletsHandler <<< over1 \(InletPath _ alias) -> alias)
    OutletsFlow outletsFlow <-
        view (_nodeOutletsFlow $ UUID.ToNode nodeUuid) nw
            # exceptMaybe (RpdError "")
    outletsCanceler :: Canceler <-
        liftEffect $ E.subscribe outletsFlow
            (outletsHandler <<< over1 \(OutletPath _ alias) -> alias)
    pure $ inletsCanceler <> inletsCanceler


connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
-- TODO: rewrite for the case of different patches
connect outletPath inletPath nw = do
    (uuid :: UUID.UUID) <- liftEffect $ liftEffect UUID.new
    (ouuid :: UUID.UUID) <-
        view (_pathToId $ ToOutlet outletPath) nw
            # exceptMaybe (RpdError "")
    (iuuid :: UUID.UUID) <-
        view (_pathToId $ ToInlet inletPath) nw
            # exceptMaybe (RpdError "")

    let
        inletUuid = UUID.ToInlet iuuid
        outletUuid = UUID.ToOutlet ouuid

    (OutletFlow outletFlow) <-
        view (_outletFlow outletUuid) nw # exceptMaybe (RpdError "")
    (InletFlow inletFlow) <-
        view (_inletFlow inletUuid) nw # exceptMaybe (RpdError "")
    (PushToInlet pushToInlet) <-
        view (_inletPush inletUuid) nw # exceptMaybe (RpdError "")

    let
        newLink = Link uuid ( outletUuid /\ inletUuid )
        -- iNodePath = getNodeOfInlet inletPath
        -- oPatchPath = getPatchOfOutlet outletPath
        -- iPatchPath = getPatchOfInlet inletPath

    linkCanceler :: Canceler <-
            liftEffect $
                E.subscribe outletFlow pushToInlet

    pure $ nw
            # setJust (_link $ UUID.ToLink uuid) newLink
            # setJust (_cancelers uuid) [ linkCanceler ]


removeLinks
    :: forall d
     . Set LinkPath
    -> Network d
    -> Rpd (Network d)
removeLinks linksForDeletion nw =
    let
        linksIdsForDeletion :: List UUID.UUID
        linksIdsForDeletion =
            Set.toUnfoldable linksForDeletion
                <#> ToLink
                <#> (\linkPath -> view (_pathToId linkPath) nw)
                 #  List.catMaybes
            -- FIXME: every `catMaybes` occurence is skipping the error, we                          -- shouldn't skip!
    in
        removeLinks' (Set.fromFoldable linksIdsForDeletion) nw


removeLinks'
    :: forall d
     . Set UUID.UUID
    -> Network d
    -> Rpd (Network d)
removeLinks' linksForDeletion nw = do
    _ <- liftEffect $ traverse_
            (\uuid ->
                view (_cancelers uuid) nw
                    # fromMaybe []
                    # traverse_ liftEffect
            )
            linksForDeletion
    pure $ (
        foldr (\linkUuid nw' ->
            nw' # set (_link $ UUID.ToLink linkUuid) Nothing
                # set (_cancelers linkUuid) Nothing
        ) nw linksForDeletion
        -- # setJust (_inletConnections inletPath) newInletConnections
        -- # setJust (_outletConnections outletPath) newOutletConnections
    )
    -- TODO: un-subscribe `process`` function of the target node to update values including this connection


disconnectAll
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
disconnectAll outletPath inletPath
    nw@(Network { registry }) = do
    iuuid <-
        view (_pathToId $ ToInlet inletPath) nw
            # exceptMaybe (RpdError "")
    ouuid <-
        view (_pathToId $ ToOutlet outletPath) nw
            # exceptMaybe (RpdError "")
    let
        inletUuid = UUID.ToInlet iuuid
        outletUuid = UUID.ToOutlet ouuid
        linkForDeletion (Link _ ( outletUuid' /\ inletUuid')) =
            (outletUuid' == outletUuid) && (inletUuid' == inletUuid)
        linksForDeletion :: List UUID.UUID
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Set.fromFoldable linksForDeletion) nw


disconnectAllComingFrom
    :: forall d
     . OutletPath
    -> Network d
    -> Rpd (Network d)
disconnectAllComingFrom outletPath
    nw@(Network { registry }) = do
    uuid <-
        view (_pathToId $ ToOutlet outletPath) nw
            # exceptMaybe (RpdError "")
    let
        outletUuid = UUID.ToOutlet uuid
    let
        linkForDeletion (Link _ ( outletUuid' /\ _)) = (outletUuid' == outletUuid)
        linksForDeletion :: List UUID.UUID
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Set.fromFoldable linksForDeletion) nw


disconnectAllComingTo
    :: forall d
     . InletPath
    -> Network d
    -> Rpd (Network d)
disconnectAllComingTo inletPath
    nw@(Network { registry }) = do
    uuid <-
        view (_pathToId $ ToInlet inletPath) nw
            # exceptMaybe (RpdError "")
    let
        inletUuid = UUID.ToInlet uuid
        linkForDeletion (Link _ ( _ /\ inletUuid')) = (inletUuid' == inletUuid)
        linksForDeletion :: List UUID.UUID
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Set.fromFoldable linksForDeletion) nw


disconnectTop
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
disconnectTop outletPath inletPath nw
    -- FIXME: implement
    = pure nw


-- TODO: disconnectTopOf (OutletPath /\ InletPath)

-- TODO: subscribeAllNodes

-- TODO: subscribeAllData


updateNodeProcessFlow
    :: forall d
     . UUID.ToNode
    -> Network d
    -> Rpd (Network d)
updateNodeProcessFlow (UUID.ToNode uuid) nw = do
    -- cancel the previous subscription if it exists
    _ <- view (_cancelers uuid) nw
            # fromMaybe []
            # traverse_ liftEffect
    (Node _ _ process { inletsFlow, inlets, outlets }) <-
        except $ view (_node $ UUID.ToNode uuid) nw # note (RpdError "")
    case process of
        Withhold -> pure nw
        -- TODO: it is OK now to join this handler and `buildOutletsFlow` in one function
        processF ->
            if (Set.isEmpty inlets || Set.isEmpty outlets) then pure nw else do
                let
                    (outletFlows :: UUID.UUID /-> PushToOutlet d) =
                        outlets
                            # (Set.toUnfoldable :: forall a. Set a -> List a)
                            # map (\(UUID.ToOutlet ouuid) ->
                                view (_outletPush $ UUID.ToOutlet ouuid) nw
                                    <#> \push -> ouuid /\ push)
                            # List.catMaybes -- FIXME: raise an error if outlet wasn't found
                            # Map.fromFoldable
                    pushToOutletFlow :: (OutletPath /\ UUID.UUID /\ d) -> Effect Unit
                    pushToOutletFlow (_ /\ ouuid /\ d) =
                        case Map.lookup ouuid outletFlows of
                            Just (PushToOutlet push) -> push d
                            Nothing -> pure unit
                OutletsFlow outletsFlow /\ maybeCancelBuild <-
                    buildOutletsFlow (UUID.ToNode uuid) processF inletsFlow inlets outlets nw
                canceler :: Canceler
                    <- liftEffect $ E.subscribe outletsFlow pushToOutletFlow
                let
                    cancelers =
                        case maybeCancelBuild of
                            Just buildCanceler -> [ canceler, buildCanceler ]
                            Nothing -> [ canceler ]
                pure $ nw # setJust (_cancelers uuid) cancelers


buildOutletsFlow
    :: forall d
     . UUID.ToNode -- FIXME: we don't use UUID here
    -> ProcessF d
    -> InletsFlow d
    -> Set UUID.ToInlet
    -> Set UUID.ToOutlet
    -> Network d
    -> Rpd (OutletsFlow d /\ Maybe Canceler) -- FIXME: for now, we only need Rpd to handle the
buildOutletsFlow _ Withhold _ _ _ _ =
    -- liftEffect never >>= pure <<< OutletsFlow
    liftEffect never >>= \flow ->
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
    { push, event } <- liftEffect E.create
    let
        -- receive = ?wh
        -- send = ?wh
        outletsAliases :: List (OutletPath /\ UUID.UUID)
        outletsAliases =
            (outlets
                 # List.fromFoldable
                <#> \uuid ->
                    view (_outlet uuid) nw
                        <#> \(Outlet uuid' path _) -> path /\ uuid')
                 # List.catMaybes
                -- FIXME: if outlet wasn't found, raise an error?
                --  # Map.fromFoldable
        foldingF ((InletPath nodePath inletAlias) /\ uuid /\ curD) inletVals =
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
            let receive = flip Map.lookup $ inletsValues
            (send :: Alias -> Maybe d) <- processNode receive
            _ <- traverse
                    (\(path@(OutletPath _ alias) /\ uuid) ->
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
    -- case processF $ InletsByIndexFlow inletsFlow of
    --     OutletsByIndexFlow outletsByIndex ->
    --         pure $ (OutletsFlow $ Just <$> outletsByIndex)
    --                /\ Nothing


-- buildOutletsFlow _ (ByLabel processF) (InletsFlow inletsFlow) inlets outlets nw =
--     let
--         inletLabels = extractInletLabels inlets nw
--         outletLabels = extractOutletLabels outlets nw
--         mapInletFlow (inletIdx /\ d) =
--             case inletLabels !! inletIdx of
--                 Just label -> (Just label /\ d)
--                 _ -> Nothing /\ d
--         mapOutletFlow maybeData =
--             maybeData
--                 >>= \(label /\ d) -> outletLabels # Array.elemIndex label
--                 <#> \maybeIdx -> maybeIdx /\ d
--         labeledInletsFlow = mapInletFlow <$> inletsFlow
--         OutletsByLabelFlow labeledOutletsFlow =
--             processF $ InletsByLabelFlow labeledInletsFlow
--     in pure $ (OutletsFlow $ mapOutletFlow <$> labeledOutletsFlow)
--               /\ Nothing
-- buildOutletsFlow nodePath (ByPath processF) (InletsFlow inletsFlow) inlets outlets _ =
--     let
--         mapInletFlow (inletIdx /\ d) =
--             Just (InletPath nodePath inletIdx) /\ d
--         inletsWithPathFlow = mapInletFlow <$> inletsFlow
--         outletsWithPathFlow = processF inletsWithPathFlow
--         mapOutletFlow maybeData =
--             maybeData
--                 <#> \((OutletPath _ outletIdx) /\ d) ->
--                     outletIdx /\ d
--     in pure $ (OutletsFlow $ mapOutletFlow <$> outletsWithPathFlow)
--               /\ Nothing
-- buildOutletsFlow _ (FoldedByIndex processF) (InletsFlow inletsFlow) inlets _ _ = do
--     -- TODO: generalize to Foldable
--     { event, push } <- liftEffect E.create
--     let
--         foldingF (curInletIdx /\ curD) inletVals =
--             Array.updateAt curInletIdx (Just curD) inletVals
--                 # fromMaybe inletVals
--         inletsFlow' = E.fold foldingF inletsFlow
--             $ Array.replicate (Set.size inlets) Nothing
--     cancel <- liftEffect $ E.subscribe inletsFlow' $ \inletsVals ->
--         let (OutletsData outletVals) = processF $ InletsData inletsVals
--         in forWithIndex outletVals \idx val ->
--             push $ Just $ idx /\ val
--     pure $ OutletsFlow event
--            /\ Just cancel
-- buildOutletsFlow _ (FoldedByLabel processF) (InletsFlow inletsFlow) inlets outlets nw = do
--     -- TODO: generalize to Foldable
--     { event, push } <- liftEffect E.create
--     let
--         inletLabels = extractInletLabels inlets nw
--         outletLabels = extractOutletLabels outlets nw
--         foldingF (curInletIdx /\ curD) inletVals =
--             case inletLabels !! curInletIdx of
--                 Just label -> inletVals # Map.insert label curD
--                 _ -> inletVals
--         inletsFlow' = E.fold foldingF inletsFlow Map.empty
--         adaptOutletVals :: (String /-> d) -> Array (Maybe (OutletInNode /\ d))
--         adaptOutletVals ouletVals =
--             Map.toUnfoldable ouletVals
--                 <#> \(label /\ d) ->
--                     outletLabels # Array.elemIndex label
--                 <#> flip (/\) d
--     cancel <- liftEffect $ E.subscribe inletsFlow' $ \inletsVals ->
--         let (OutletsMapData outletVals) = processF $ InletsMapData inletsVals
--         in traverse push $ adaptOutletVals outletVals
--     pure $ OutletsFlow event
--            /\ Just cancel

    -- TODO: may be, request these functions from user:
    --   for given inlet (path?), get its map key
    --   for given outlet (path?), get its map key
    --   for given key, get the corresponding inlet path
    --   for given key, get the corresponding outlet path


joinCancelers :: Canceler -> Canceler -> Canceler
joinCancelers = (<>)


extractInletLabels :: forall d. Set InletPath → Network d → Array Alias
extractInletLabels inlets nw =
    inlets
        # (Set.toUnfoldable :: forall a. Set a -> Array a)
        # map (\inletPath -> view (_inletByPath inletPath) nw)
        # E.filterMap identity -- FIXME: raise an error if outlet wasn't found
        # map (\(Inlet _ path _) -> mkAlias "") -- FIXME: real alias


extractOutletLabels :: forall d. Set OutletPath → Network d → Array Alias
extractOutletLabels outlets nw =
    outlets
        # (Set.toUnfoldable :: forall a. Set a -> Array a)
        # map (\outletPath -> view (_outletByPath outletPath) nw)
        # E.filterMap identity -- FIXME: raise an error if outlet wasn't found
        # map (\(Outlet _ path _) -> mkAlias "") -- FIXME: real alias


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

