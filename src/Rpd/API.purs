
module Rpd.API
    ( Rpd, RpdError, init
    , (</>), andThen
    , connect, disconnectAll, disconnectTop
    , addPatch, addNode, addInlet, addOutlet
    , addToolkitNode, addDefNode
    , removeInlet
    , subscribeInlet, subscribeOutlet, subscribeAllInlets, subscribeAllOutlets
    , subscribeChannelsData, subscribeNode  -- subscribeAllData
    , subscribeInlet', subscribeOutlet', subscribeAllInlets', subscribeAllOutlets'
    , subscribeChannelsData', subscribeNode'  -- subscribeAllData'
    , sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
    -- , findPatch, findNode, findOutlet, findInlet
    ) where

import Debug.Trace

import Prelude

import Control.Monad.Except.Trans (ExceptT, except)

--import Data.Array ((!!), (:), snoc)
import Data.Array (snoc)
-- import Data.Array as Array
import Data.Bitraversable (bisequence)
import Data.Either (Either(..), note)
import Data.Foldable (foldr)
import Data.Lens (view, set, setJust)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
--import Data.Traversable (for, sequence, traverse, traverse_)
import Data.Traversable (traverse, traverse_)
-- import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\), type (/\), over1)

--import Effect (Effect, foreachE)
import Effect (Effect)
import Effect.Class (liftEffect)

import FRP.Event as E

import Rpd.Network
import Rpd.Network (empty) as Network
import Rpd.UUID as UUID
-- import Rpd.UUID (UUID)
import Rpd.Util (type (/->), PushableFlow(..), Subscriber, Canceler, Flow, never)
-- import Rpd.Util as RU
import Rpd.Optics
-- import Rpd.Path (Path)
import Rpd.Path as Path
import Rpd.Process (InletAlias, InletHandler(..), OutletAlias, OutletHandler(..), ProcessF(..))
import Rpd.Toolkit (Toolkit(..))
import Rpd.Toolkit as Toolkit


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


someApiFunc :: forall d c. Rpd (Network d c String)
someApiFunc =
    init "test"
        </> addPatch "foo"
        </> addNode (Path.toPatch "foo") "test1" ""
        </> addNode (Path.toPatch "foo") "test2" ""


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


init :: forall d c n. String -> Rpd (Network d c n)
init = pure <<< Network.empty


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
    except $ if bool then Right unit else Left err


uuidByPath
    :: forall p x d c n
     . Path.MarksPath p
    => (UUID.Tagged -> Maybe x)
    -> p
    -> Network d c n
    -> Rpd x
uuidByPath f path nw = do
    (uuid' :: UUID.Tagged) <- view (_pathToId $ Path.lift path) nw # exceptMaybe (RpdError "")
    (uuid :: x) <- f uuid' # exceptMaybe (RpdError "")
    pure uuid


addPatch :: forall d c n. Path.Alias -> Network d c n -> Rpd (Network d c n)
addPatch alias nw = do
    uuid <- liftEffect UUID.new
    let
        path = Path.toPatch alias
        newPatch =
            Patch
                (UUID.ToPatch uuid)
                path
                { nodes : Seq.empty
                , links : Seq.empty
                }
    pure $ nw
        # setJust (_patch $ UUID.ToPatch uuid) newPatch
        # setJust (_pathToId $ Path.lift path) (UUID.liftTagged $ UUID.ToPatch uuid)
        # setJust (_networkPatch $ UUID.ToPatch uuid) unit


-- TODO: removePatch
    -- TODO: cancel all the cancelers related to the patch


addNode
    :: forall d c n
     . Path.ToPatch
    -> Path.Alias
    -> n
    -> Network d c n
    -> Rpd (Network d c n)
addNode patchPath nodeAlias n nw = do
    patchUuid <- nw # uuidByPath UUID.toPatch patchPath
    uuid <- liftEffect UUID.new
    PushableFlow pushToInlets inletsFlow <- liftEffect makePushableFlow
    PushableFlow pushToOutlets outletsFlow <- liftEffect makePushableFlow
    let
        path = Path.nodeInPatch patchPath nodeAlias
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
    nw
         #  setJust (_node $ UUID.ToNode uuid) newNode
         #  setJust (_pathToId $ Path.lift path) (UUID.liftTagged $ UUID.ToNode uuid)
         #  setJust (_patchNode patchUuid $ UUID.ToNode uuid) unit
        --  #  addInlets nodePath def.inletDefs
        -- </> addOutlets nodePath def.outletDefs
         # updateNodeProcessFlow (UUID.ToNode uuid)


addToolkitNode
    :: forall d c n
     . Toolkit.Channels d c
    => Path.ToPatch
    -> Path.Alias
    -> Toolkit d c n
    -> n
    -> Network d c n
    -> Rpd (Network d c n)
addToolkitNode patchPath nodeAlias (Toolkit name toolkitF) n nw = do
    -- FIXME: may be it should be default, so we always require toolkit?
    --        since it may confuse the user that when she has toolkit defined
    --        somewhere then adding the node of the type is not enough
    -- ... Or the Toolkit should always be the part of the Network --> Then remove this function
    nw # addDefNode patchPath nodeAlias (toolkitF n) n


addDefNode
    :: forall d c n
     . Toolkit.Channels d c
    => Path.ToPatch
    -> Path.Alias
    -> Toolkit.NodeDef d c
    -> n
    -> Network d c n
    -> Rpd (Network d c n)
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
        addInlet' (Toolkit.InletAlias inletAlias /\ channel) rpd =
            rpd </>
                addInlet path inletAlias channel
        addOutlet' (Toolkit.OutletAlias outletAlias /\ channel) rpd =
            rpd </>
                addOutlet path outletAlias channel


processWith
    :: forall d c n
     . Path.ToNode
    -> ProcessF d
    -> Network d c n
    -> Rpd (Network d c n)
processWith path processF nw = do
    uuid <- nw # uuidByPath UUID.toNode path
    (Node _ path n _ state) :: Node d n <-
        view (_node uuid) nw
            # exceptMaybe (RpdError "")
    let
        newNode =
            Node
                uuid
                path
                n
                processF
                state
    nw
        # setJust (_node uuid) newNode
        # updateNodeProcessFlow uuid


addInlet
    :: forall d c n
     . Path.ToNode
    -> Path.Alias
    -> c
    -> Network d c n
    -> Rpd (Network d c n)
addInlet nodePath alias c nw = do
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    uuid <- liftEffect UUID.new
    PushableFlow pushToInlet inletFlow <- liftEffect makePushableFlow
    (Node _ _ _ _ { pushToInlets }) :: Node d n
        <- view (_node nodeUuid) nw
            # exceptMaybe (RpdError "")
    let
        path = Path.inletInNode nodePath alias
        (PushToInlets informNode) = pushToInlets
        newInlet =
            Inlet
                (UUID.ToInlet uuid)
                path
                c
                { flow : InletFlow inletFlow
                , push : PushToInlet pushToInlet
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe inletFlow (\d -> informNode (path /\ (UUID.ToInlet uuid) /\ d))
    -- userCancelers :: Array Canceler <-
    --     liftEffect $ traverse (E.subscribe dataFlow) subs
    nw # setJust (_inlet $ UUID.ToInlet uuid) newInlet
       # setJust (_pathToId $ Path.lift path) (UUID.liftTagged $ UUID.ToInlet uuid)
       # setJust (_nodeInlet nodeUuid (UUID.ToInlet uuid)) unit
       # setJust (_cancelers uuid) [ canceler ]
       # updateNodeProcessFlow nodeUuid


-- addInlets :: forall d. List Path.ToInlet -> Network d -> Rpd (Network d)
-- addInlets nodePath inletDefs nw =
--     -- FIXME: may appear not very optimal, since every `addInlet'`
--     --        call looks for the node again and again
--     foldr foldingF (pure nw) inletDefs
--     where
--         foldingF inletDef rpd =
--             rpd </> addInlet' nodePath inletDef


removeInlet
    :: forall d c n
     . Path.ToInlet
    -> Network d c n
    -> Rpd (Network d c n)
removeInlet path nw = do
    nodePath <- Path.getNodePath (Path.lift path)
        # exceptMaybe (RpdError "")
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    uuid <- nw # uuidByPath UUID.toInlet path
    -- _ <- view (_inlet inletPath) nw # exceptMaybe (RpdError "")
    -- let (InletPath nodePath inletIdx) = inletPath
    view (_cancelers $ UUID.uuid uuid) nw
        # fromMaybe []
        # traverse_ liftEffect
    nw  #  set (_inlet uuid) Nothing
        #  set (_pathToId $ Path.lift path) Nothing
        #  set (_nodeInlet nodeUuid uuid) Nothing
        #  setJust (_cancelers $ UUID.uuid uuid) [ ]
        #  disconnectAllComingTo path
       </> updateNodeProcessFlow nodeUuid


addOutlet
    :: forall d c n
     . Path.ToNode
    -> Path.Alias
    -> c
    -> Network d c n
    -> Rpd (Network d c n)
addOutlet nodePath alias c nw = do
    nodeUuid <- nw # uuidByPath UUID.toNode nodePath
    uuid <- liftEffect UUID.new
    PushableFlow pushToOutlet outletFlow <- liftEffect makePushableFlow
    (Node _ _ _ _ { pushToOutlets }) :: Node d n
        <- view (_node nodeUuid) nw # exceptMaybe (RpdError "")
    let
        path = Path.outletInNode nodePath alias
        (PushToOutlets informNode) = pushToOutlets
        newOutlet =
            Outlet
                (UUID.ToOutlet uuid)
                path
                c
                { flow : OutletFlow outletFlow
                , push : PushToOutlet pushToOutlet
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe outletFlow (\d -> informNode (path /\ (UUID.ToOutlet uuid) /\ d))
    nw # setJust (_outlet $ UUID.ToOutlet uuid) newOutlet
       # setJust (_pathToId $ Path.lift path) (UUID.liftTagged $ UUID.ToOutlet uuid)
       # setJust (_nodeOutlet nodeUuid $ UUID.ToOutlet uuid) unit
       # setJust (_cancelers uuid) [ canceler ]
       # updateNodeProcessFlow nodeUuid


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
    :: forall d c n
     . Path.ToInlet
    -> d
    -> Network d c n
    -> Rpd (Network d c n)
sendToInlet path d nw = do
    uuid <- nw # uuidByPath UUID.toInlet path
    (PushToInlet push) <-
        view (_inletPush uuid) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToInlet
    :: forall d c n
     . Path.ToInlet
    -> Flow d
    -> Network d c n
    -> Rpd Canceler
streamToInlet path flow nw = do
    uuid <- nw # uuidByPath UUID.toInlet path
    (PushToInlet push) <-
        view (_inletPush uuid) nw # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


sendToOutlet -- TODO: consider removing?
    :: forall d c n
     . Path.ToOutlet
    -> d
    -> Network d c n
    -> Rpd (Network d c n)
sendToOutlet path d nw = do
    uuid <- nw # uuidByPath UUID.toOutlet path
    (PushToOutlet push) <-
        view (_outletPush uuid) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToOutlet -- TODO: consider removing?
    :: forall d c n
     . Path.ToOutlet
    -> Flow d
    -> Network d c n
    -> Rpd Canceler
streamToOutlet path flow nw = do
    uuid <- nw # uuidByPath UUID.toOutlet path
    (PushToOutlet push) <-
        view (_outletPush uuid) nw # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


subscribeInlet
    :: forall d c n
     . Path.ToInlet
    -> InletHandler d
    -> Network d c n
    -> Rpd (Network d c n)
subscribeInlet path (InletHandler handler) nw = do
    uuid <- nw # uuidByPath UUID.toInlet path
    (InletFlow flow) <-
        view (_inletFlow uuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    curCancelers <-
        view (_cancelers $ UUID.uuid uuid) nw
            # exceptMaybe (RpdError "")
    pure $
        nw # setJust (_cancelers $ UUID.uuid uuid) (curCancelers +> canceler)


subscribeInlet'
    :: forall d c n
     . Path.ToInlet
    -> InletHandler d
    -> Network d c n
    -> Rpd Canceler
subscribeInlet' path (InletHandler handler) nw = do
    uuid <- nw # uuidByPath UUID.toInlet path
    (InletFlow flow) <-
        view (_inletFlow uuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeOutlet
    :: forall d c n
     . Path.ToOutlet
    -> OutletHandler d
    -> Network d c n
    -> Rpd (Network d c n)
subscribeOutlet path (OutletHandler handler) nw = do
    uuid <- nw # uuidByPath UUID.toOutlet path
    (OutletFlow flow) <-
        view (_outletFlow uuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    curCancelers <-
        view (_cancelers $ UUID.uuid uuid) nw
            # exceptMaybe (RpdError "")
    pure $
        nw # setJust (_cancelers $ UUID.uuid uuid) (curCancelers +> canceler)


subscribeOutlet'
    :: forall d c n
     . Path.ToOutlet
    -> OutletHandler d
    -> Network d c n
    -> Rpd Canceler
subscribeOutlet' path (OutletHandler handler) nw = do
    uuid <- nw # uuidByPath UUID.toOutlet path
    (OutletFlow flow) <-
        view (_outletFlow uuid) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeAllInlets
    :: forall d c n
     . (Path.ToInlet -> d -> Effect Unit)
    -> Network d c n
    -> Rpd (Network d c n)
subscribeAllInlets handler nw = do
    _ <- liftEffect $ subscribeAllInlets' handler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeAllInlets'
    :: forall d c n
     . (Path.ToInlet -> d -> Effect Unit)
    -> Network d c n
    -> Effect (Path.ToInlet /-> Canceler)
subscribeAllInlets' handler nw = do
    let
        inlets :: List (Inlet d c)
        inlets = view _networkInlets nw
        pathOfInlet (Inlet _ inletPath _ _) = inletPath
        inletsPaths :: List Path.ToInlet
        inletsPaths = pathOfInlet <$> inlets
    cancelers :: List Canceler <- traverse sub inlets
    pure $ Map.fromFoldable $ (/\) <$> inletsPaths <*> cancelers
    where
        sub :: Inlet d c -> Subscriber
        sub (Inlet _ inletPath _ { flow }) =
            case flow of
                InletFlow inletFlow -> E.subscribe inletFlow $ handler inletPath


subscribeAllOutlets
    :: forall d c n
     . (Path.ToOutlet -> d -> Effect Unit)
    -> Network d c n
    -> Rpd (Network d c n)
subscribeAllOutlets handler nw = do
    _ <- liftEffect $ subscribeAllOutlets' handler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeAllOutlets'
    :: forall d c n
     . (Path.ToOutlet -> d -> Effect Unit)
    -> Network d c n
    -> Effect (Path.ToOutlet /-> Canceler)
subscribeAllOutlets' handler nw = do
    let
        outlets :: List (Outlet d c)
        outlets = view _networkOutlets nw
        pathOfOutlet (Outlet _ outletPath _ _) = outletPath
        outletsPaths :: List Path.ToOutlet
        outletsPaths = pathOfOutlet <$> outlets
    cancelers :: List Canceler  <- traverse sub outlets
    pure $ Map.fromFoldable $ (/\) <$> outletsPaths <*> cancelers
    where
        sub :: Outlet d c -> Subscriber
        sub (Outlet _ outletPath _ { flow }) =
            case flow of
                OutletFlow outletFlow -> E.subscribe outletFlow $ handler outletPath


subscribeChannelsData
    :: forall d c n
     . (Path.ToOutlet -> d -> Effect Unit)
    -> (Path.ToInlet -> d -> Effect Unit)
    -> Network d c n
    -> Rpd (Network d c n)
subscribeChannelsData oHandler iHandler nw = do
    _ <- liftEffect $ subscribeChannelsData' oHandler iHandler nw
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeChannelsData'
    :: forall d c n
     . (Path.ToOutlet -> d -> Effect Unit)
    -> (Path.ToInlet -> d -> Effect Unit)
    -> Network d c n
    -> Effect ((Path.ToOutlet /-> Canceler) /\ (Path.ToInlet /-> Canceler))
subscribeChannelsData' oHandler iHandler nw =
    bisequence $ subscribeAllOutlets' oHandler nw /\ subscribeAllInlets' iHandler nw


subscribeNode
    :: forall d c n
     . Path.ToNode
    -> (InletAlias /\ UUID.ToInlet /\ d -> Effect Unit)
    -> (OutletAlias /\ UUID.ToOutlet /\ d -> Effect Unit)
    -> Network d c n
    -> Rpd (Network d c n)
subscribeNode nodePath inletsHandler outletsHandler nw = do
    _ <- subscribeNode' nodePath inletsHandler outletsHandler nw
    -- FIXME: implement !!!!
    -- FIXME: implement storing the cancellers to execute them on remove
    pure nw


subscribeNode'
    :: forall d c n
     . Path.ToNode
    -> (InletAlias /\ UUID.ToInlet /\ d -> Effect Unit)
    -> (OutletAlias /\ UUID.ToOutlet /\ d -> Effect Unit)
    -> Network d c n
    -> Rpd Canceler
subscribeNode' path inletsHandler outletsHandler nw = do
    uuid <- uuidByPath UUID.toNode path nw
    InletsFlow inletsFlow <-
        view (_nodeInletsFlow uuid) nw
            # exceptMaybe (RpdError "")
    inletsCanceler :: Canceler <-
        liftEffect $ E.subscribe inletsFlow
            (inletsHandler <<< over1 \(Path.ToInlet { inlet }) -> inlet)
    OutletsFlow outletsFlow <-
        view (_nodeOutletsFlow uuid) nw
            # exceptMaybe (RpdError "")
    outletsCanceler :: Canceler <-
        liftEffect $ E.subscribe outletsFlow
            (outletsHandler <<< over1 \(Path.ToOutlet { outlet }) -> outlet)
    pure $ inletsCanceler <> inletsCanceler


connect
    :: forall d c n
     . Path.ToOutlet
    -> Path.ToInlet
    -> Network d c n
    -> Rpd (Network d c n)
-- TODO: rewrite for the case of different patches
connect outletPath inletPath nw = do
    (uuid :: UUID.UUID) <- liftEffect $ liftEffect UUID.new
    ouuid <- uuidByPath UUID.toOutlet outletPath nw
    iuuid <- uuidByPath UUID.toInlet inletPath nw
    -- FIXME: ensure that inlet and outlet are from the same patch

    (OutletFlow outletFlow) <-
        view (_outletFlow ouuid) nw # exceptMaybe (RpdError "")
    (InletFlow inletFlow) <-
        view (_inletFlow iuuid) nw # exceptMaybe (RpdError "")
    (PushToInlet pushToInlet) <-
        view (_inletPush iuuid) nw # exceptMaybe (RpdError "")

    let
        newLink = Link (UUID.ToLink uuid) { outlet : ouuid, inlet : iuuid }
        patchPath = Path.getPatchPath $ Path.lift outletPath
        -- iNodePath = getNodeOfInlet inletPath
        -- oPatchPath = getPatchOfOutlet outletPath
        -- iPatchPath = getPatchOfInlet inletPath

    patchUuid <- uuidByPath UUID.toPatch patchPath nw
    linkCanceler :: Canceler <-
            liftEffect $
                E.subscribe outletFlow pushToInlet

    pure $ nw
            # setJust (_link $ UUID.ToLink uuid) newLink
            # setJust (_patchLink patchUuid $ UUID.ToLink uuid) unit
            # setJust (_cancelers uuid) [ linkCanceler ]


-- removeLinks
--     :: forall d
--      . Set UUID.ToLink
--     -> Network d
--     -> Rpd (Network d)
-- removeLinks linksForDeletion nw =
--     let
--         linksIdsForDeletion :: List UUID.UUID
--         linksIdsForDeletion =
--             Set.toUnfoldable linksForDeletion
--                 <#> ToLink
--                 <#> (\linkPath -> view (_pathToId linkPath) nw)
--                  #  List.catMaybes
--             -- FIXME: every `catMaybes` occurence is skipping the error, we                          -- shouldn't skip!
--     in
--         removeLinks' (Set.fromFoldable linksIdsForDeletion) nw


removeLinks'
    :: forall d c n
     . Seq UUID.ToLink
    -> Network d c n
    -> Rpd (Network d c n)
removeLinks' linksForDeletion nw = do
    _ <- liftEffect $ traverse_
            (\uuid ->
                view (_cancelers $ UUID.uuid uuid) nw
                    # fromMaybe []
                    # traverse_ liftEffect
            )
            linksForDeletion
    pure $ (
        foldr (\linkUuid nw' ->
            nw' # set (_link linkUuid) Nothing
                # set (_cancelers $ UUID.uuid linkUuid) Nothing
        ) nw linksForDeletion
        -- # setJust (_inletConnections inletPath) newInletConnections
        -- # setJust (_outletConnections outletPath) newOutletConnections
    )
    -- TODO: un-subscribe `process`` function of the target node to update values including this connection


disconnectAll
    :: forall d c n
     . Path.ToOutlet
    -> Path.ToInlet
    -> Network d c n
    -> Rpd (Network d c n)
disconnectAll outletPath inletPath
    nw@(Network { registry }) = do
    -- FIXME: delete links not from all the network but inside the specific patch
    --        (so even don't use the new `_networkLinks` lens)
    ouuid <- uuidByPath UUID.toOutlet outletPath nw
    iuuid <- uuidByPath UUID.toInlet inletPath nw
    let
        linkForDeletion (Link _ { outlet : ouuid', inlet : iuuid' }) =
            (ouuid' == ouuid) && (iuuid' == iuuid)
        linksForDeletion :: List UUID.ToLink
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Seq.fromFoldable linksForDeletion) nw


disconnectAllComingFrom
    :: forall d c n
     . Path.ToOutlet
    -> Network d c n
    -> Rpd (Network d c n)
disconnectAllComingFrom path
    nw@(Network { registry }) = do
    -- FIXME: delete links not from all the network but inside the specific patch
    --        (so even don't use the new `_networkLinks` lens)
    uuid <- uuidByPath UUID.toOutlet path nw
    let
        linkForDeletion (Link _ { outlet : uuid' }) = (uuid' == uuid)
        linksForDeletion :: List UUID.ToLink
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Seq.fromFoldable linksForDeletion) nw


disconnectAllComingTo
    :: forall d c n
     . Path.ToInlet
    -> Network d c n
    -> Rpd (Network d c n)
disconnectAllComingTo path
    nw@(Network { registry }) = do
    -- FIXME: delete links not from all the network but inside the specific patch
    --        (so even don't use the new `_networkLinks` lens)
    uuid <- uuidByPath UUID.toInlet path nw
    let
        linkForDeletion (Link _ { inlet : uuid' }) = (uuid' == uuid)
        linksForDeletion :: List UUID.ToLink
        linksForDeletion =
            Map.values registry
                # List.mapMaybe extractLink
                # List.filter linkForDeletion
                # map \(Link linkUuid _) -> linkUuid
    removeLinks' (Seq.fromFoldable linksForDeletion) nw


disconnectTop
    :: forall d c n
     . Path.ToOutlet
    -> Path.ToInlet
    -> Network d c n
    -> Rpd (Network d c n)
disconnectTop outletPath inletPath nw
    -- FIXME: implement
    = pure nw


-- TODO: disconnectTopOf (OutletPath /\ InletPath)

-- TODO: subscribeAllNodes

-- TODO: subscribeAllData


updateNodeProcessFlow
    :: forall d c n
     . UUID.ToNode
    -> Network d c n
    -> Rpd (Network d c n)
updateNodeProcessFlow (UUID.ToNode uuid) nw = do
    -- cancel the previous subscription if it exists
    _ <- view (_cancelers uuid) nw
            # fromMaybe []
            # traverse_ liftEffect
    (Node _ _ _ process { inletsFlow, inlets, outlets }) <-
        except $ view (_node $ UUID.ToNode uuid) nw # note (RpdError "")
    case process of
        Withhold -> pure nw
        -- TODO: it is OK now to join this handler and `buildOutletsFlow` in one function
        processF ->
            if (Seq.null inlets || Seq.null outlets) then pure nw else do
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
    :: forall d c n
     . UUID.ToNode -- FIXME: we don't use UUID here
    -> ProcessF d
    -> InletsFlow d
    -> Seq UUID.ToInlet
    -> Seq UUID.ToOutlet
    -> Network d c n
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


extractInletLabels :: forall d c n. Seq Path.ToInlet → Network d c n → Array Path.Alias
extractInletLabels inlets nw =
    inlets
        # (Seq.toUnfoldable :: forall a. Seq a -> Array a)
        # map (\inletPath -> view (_inletByPath inletPath) nw)
        # E.filterMap identity -- FIXME: raise an error if outlet wasn't found
        # map (\(Inlet _ (Path.ToInlet { inlet }) _ _) -> inlet) -- FIXME: real alias


extractOutletLabels :: forall d c n. Seq Path.ToOutlet → Network d c n → Array Path.Alias
extractOutletLabels outlets nw =
    outlets
        # (Seq.toUnfoldable :: forall a. Seq a -> Array a)
        # map (\outletPath -> view (_outletByPath outletPath) nw)
        # E.filterMap identity -- FIXME: raise an error if outlet wasn't found
        # map (\(Outlet _ (Path.ToOutlet { outlet }) _ _) -> outlet)


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

