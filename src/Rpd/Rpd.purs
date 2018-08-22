module Rpd
    ( Rpd, RpdError, init
    , {- (</>), -} type (/->), {- rpdAp, -} run, emptyNetwork
    --, RpdOp, RpdEffOp
    , Flow, flow
    , Network, Patch, Node, Inlet, Outlet, Link
    , PatchDef, NodeDef, InletDef, OutletDef
    , Canceler, Subscriber, PushableFlow
    --, emptyNetwork
    --, network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    , connect, disconnectAll --, disconnectTop
    , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    , subscribeInlet, {- subscribeOutlet, -} subscribeAllData, subscribeAllInlets, subscribeAllOutlets
    , sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
    , ProcessF
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    --, findPatch, findNode, findOutlet, findInlet
    ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

--import Control.Monad.Cont.Trans (ContT(..))
-- import Control.MonadZero (guard)
-- import Control.Alternative ((<|>))

import Data.Either (Either(..), either, note)
import Data.Lens (Lens', Getter', lens, view, set, setJust, over, to)
import Data.Lens.At (at)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (fold, foldr)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Bitraversable (bisequence)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Effect.Class.Console (log)

import Control.Monad.Except.Trans (ExceptT, runExceptT)

import FRP.Event (Event)
import FRP.Event as E


--import Rpd.Flow as Flow

-- data RunningNetwork d e = RpdEff e (Network d e)

data RpdError = RpdError String


type RpdOp a = Either RpdError a
--type RpdEffOp e a = RpdEff e (RpdOp e a)
type Rpd a = ExceptT RpdError Effect a
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


{- infixl 1 rpdAp as </> -}
-- FIXME: can be replaced with proper instances?
-- other options: └, ~>, ...


run
    :: forall d
     . (RpdError -> Effect Unit)
    -> (Network d -> Effect Unit)
    -> Rpd (Network d)
    -> Effect Unit
run onError onSuccess rpd =
    runExceptT rpd >>= either onError onSuccess
    -- FIXME: we should also call all the cancelers left in the network, before "exiting"


-- FIXME: continuation monad
-- (a -> r) -> r
-- (a -> m r) -> m r
-- (a -> Eff (Except err a)) -> Eff (Except err a)
-- ContT (Except err a) Eff a
-- rpdAp :: forall a b. Rpd a -> (a -> Rpd b) -> Rpd b
-- rpdAp eff f =
--     eff >>= either (pure <<< Left) f


-- someApiFunc :: forall d. Rpd (Network d)
-- someApiFunc =
--     init "t"
--         </> addPatch "foo"
--         </> addNode (PatchId 0) "test1"
--         </> addNode (PatchId 0) "test2"


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


type Flow d = Event d
type PushF d = (d -> Effect Unit)
data PushableFlow d = PushableFlow (PushF d) (Flow d)
-- paths in connection duplicate what is stored in the link
data InletConnection = InletConnection OutletPath LinkId
data OutletConnection = OutletConnection InletPath LinkId

type Canceler =
    Effect Unit
type Subscriber =
    Effect Canceler


flow :: forall d. Event d -> Flow d
flow = identity


-- type ProcessF d = (Map InletPath d -> Map OutletPath d)
type ProcessF d = (Map String d -> Map String d)
-- type DirectProcessF d = (String -> d -> String /\ d)
-- type DirectProcessF' d = (String -> d -> d)
-- type OptionalProcessF d = (String -> d -> Maybe d)
-- type OptionalProcessF' d = (String -> d -> String /\ Maybe d)

data PatchId = PatchId Int
data NodePath = NodePath PatchId Int
data InletPath = InletPath NodePath Int
data OutletPath = OutletPath NodePath Int
data LinkId = LinkId Int


-- data DataSource d
--     = UserSource (Flow d)
--     | OutletSource OutletPath (Flow d)

type PatchDef d =
    { name :: String
    , nodeDefs :: List (NodeDef d)
    }
type NodeDef d =
    { name :: String
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: ProcessF d
    }
type InletDef d =
    { label :: String
    , default :: Maybe d
    , accept :: Maybe d
    --, adapt ::
    }
type OutletDef d =
    { label :: String
    }

infixr 6 type Map as /->


data Network d =
    Network
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: PatchId /-> Patch d
        , nodes :: NodePath /-> Node d
        , inlets :: InletPath /-> Inlet d
        , outlets :: OutletPath /-> Outlet d
        , links :: LinkId /-> Link
        , linkCancelers :: LinkId /-> Canceler
        , processCancelers :: InletPath /-> Canceler
        }
data Patch d =
    Patch
        PatchId
        (PatchDef d)
        { nodes :: Set NodePath
        , links :: Set LinkId -- do we need them here? just filter network links by patch
        }
data Node d =
    Node
        NodePath -- (NodeDef d)
        (NodeDef d)
        { inlets :: Set InletPath
        , outlets :: Set OutletPath
        , flow :: PushableFlow (InletPath /\ d)
        , processFlow :: Flow (InletPath /-> d)
        }
data Inlet d =
    Inlet
        InletPath
        (InletDef d)
        { flow :: PushableFlow d
        , connections :: List InletConnection
        -- sources :: Set (DataSource d)
        }
data Outlet d =
    Outlet
        OutletPath
        (OutletDef d)
        { flow :: PushableFlow d
        , connections :: List OutletConnection
        }
data Link = Link OutletPath InletPath


init :: forall d. String -> Rpd (Network d)
init = pure <<< emptyNetwork


emptyNetwork :: forall d. String -> Network d
emptyNetwork name =
    Network
        { name
        , patchDefs : List.Nil
        }
        { patches : Map.empty
        , nodes : Map.empty
        , inlets : Map.empty
        , outlets : Map.empty
        , links : Map.empty
        , linkCancelers : Map.empty
        , processCancelers : Map.empty
        }



_patch :: forall d. PatchId -> Lens' (Network d) (Maybe (Patch d))
_patch patchId =
    lens getter setter
    where
        patchLens = at patchId
        getter (Network _ { patches }) = view patchLens patches
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { patches = set patchLens val nwstate.patches }


_patchNode :: forall d. PatchId -> NodePath -> Lens' (Network d) (Maybe Unit)
_patchNode patchId nodePath =
    lens getter setter
    where
        patchLens = _patch patchId
        nodeLens = at nodePath
        getter nw =
            view patchLens nw
            >>= \(Patch _ _ { nodes }) -> view nodeLens nodes
        setter nw val =
            over patchLens
                (map $ \(Patch pid pdef pstate) ->
                    Patch
                        pid
                        pdef
                        pstate { nodes = set nodeLens val pstate.nodes }
                ) nw


_node :: forall d. NodePath -> Lens' (Network d) (Maybe (Node d))
_node nodePath@(NodePath patchId _) =
    lens getter setter
    where
        nodeLens = at nodePath
        getter (Network _ { nodes }) = view nodeLens nodes
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { nodes = set nodeLens val nwstate.nodes }
            -- # set (_patchNode patchId nodePath) (const unit <$> val)


_nodeInlet :: forall d. NodePath -> InletPath -> Lens' (Network d) (Maybe Unit)
_nodeInlet nodePath inletPath =
    lens getter setter
    where
        nodeLens = _node nodePath
        inletLens = at inletPath
        getter nw =
            view nodeLens nw
            >>= \(Node _ _ { inlets }) -> view inletLens inlets
        setter nw val =
            over nodeLens
                (map $ \(Node nid ndef nstate) ->
                    Node
                        nid
                        ndef
                        nstate { inlets = set inletLens val nstate.inlets }
                ) nw


_inlet :: forall d. InletPath -> Lens' (Network d) (Maybe (Inlet d))
_inlet inletPath@(InletPath nodePath _) =
    lens getter setter
    where
        inletLens = at inletPath
        getter (Network _ { inlets }) = view inletLens inlets
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { inlets = set inletLens val nwstate.inlets }
            -- # set (_nodeInlet nodePath inletPath) (const unit <$> val)


_inletPFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (PushableFlow d))
_inletPFlow inletPath =
    to extractPFlow
    where
        inletLens = _inlet inletPath
        extractPFlow nw = view inletLens nw >>=
            \(Inlet _ _ { flow }) -> pure flow


_inletFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (Flow d))
_inletFlow inletPath =
    to extractFlow
    where
        pFlowLens = _inletPFlow inletPath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_inletConnections :: forall d. InletPath -> Lens' (Network d) (Maybe (List InletConnection))
_inletConnections inletPath =
    lens getter setter
    where
        inletLens = _inlet inletPath
        getter nw =
            view inletLens nw
            >>= \(Inlet _ _ { connections }) -> pure connections
        setter nw val =
            over inletLens
                (map $ \(Inlet iid idef istate) ->
                    Inlet
                        iid
                        idef
                        istate { connections = fromMaybe' (const $ List.Nil) val }
                ) nw


_nodeOutlet :: forall d. NodePath -> OutletPath -> Lens' (Network d) (Maybe Unit)
_nodeOutlet nodePath outletPath =
    lens getter setter
    where
        nodeLens = _node nodePath
        outletLens = at outletPath
        getter nw =
            view nodeLens nw
            >>= \(Node _ _ { outlets }) -> view outletLens outlets
        setter nw val =
            over nodeLens
                (map $ \(Node nid ndef nstate) ->
                    Node
                        nid
                        ndef
                        nstate { outlets = set outletLens val nstate.outlets }
                ) nw


_outlet :: forall d e. OutletPath -> Lens' (Network d) (Maybe (Outlet d))
_outlet outletPath@(OutletPath nodePath _) =
    lens getter setter
    where
        outletLens = at outletPath
        getter (Network _ { outlets }) = view outletLens outlets
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { outlets = set outletLens val nwstate.outlets }
            -- # set (_nodeOutlet nodePath outletPath) (const unit <$> val)


_outletPFlow :: forall d. OutletPath -> Getter' (Network d) (Maybe (PushableFlow d))
_outletPFlow outletPath =
    to extractPFlow
    where
        outletLens = _outlet outletPath
        extractPFlow nw = view outletLens nw >>=
            \(Outlet _ _ { flow }) -> pure flow


_outletFlow :: forall d. OutletPath -> Getter' (Network d) (Maybe (Flow d))
_outletFlow outletPath =
    to extractFlow
    where
        pFlowLens = _outletPFlow outletPath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_outletConnections :: forall d. OutletPath -> Lens' (Network d) (Maybe (List OutletConnection))
_outletConnections outletPath =
    lens getter setter
    where
        outletLens = _outlet outletPath
        getter nw =
            view outletLens nw
            >>= \(Outlet _ _ { connections }) -> pure connections
        setter nw val =
            over outletLens
                (map $ \(Outlet oid odef ostate) ->
                    Outlet
                        oid
                        odef
                        ostate { connections = fromMaybe' (const $ List.Nil) val }
                ) nw


_patchLink :: forall d. PatchId -> LinkId -> Lens' (Network d) (Maybe Unit)
_patchLink patchId linkId =
    lens getter setter
    where
        patchLens = _patch patchId
        linkLens = at linkId
        getter nw =
            view patchLens nw
            >>= \(Patch _ _ { links }) -> view linkLens links
        setter nw val =
            over patchLens
                (map $ \(Patch pid pdef pstate) ->
                    Patch
                        pid
                        pdef
                        pstate { links = set linkLens val pstate.links }
                ) nw


_link :: forall d. LinkId -> Lens' (Network d) (Maybe Link)
_link linkId =
    lens getter setter
    where
        linkLens = at linkId
        getter (Network _ { links }) = view linkLens links
        setter nw@(Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { links = set linkLens val nwstate.links }


_canceler :: forall d. LinkId -> Lens' (Network d) (Maybe Canceler)
_canceler linkId =
    lens getter setter
    where
        cancelerLens = at linkId
        getter (Network _ { linkCancelers }) = view cancelerLens linkCancelers
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { linkCancelers = set cancelerLens val nwstate.linkCancelers }


_processCanceler :: forall d. InletPath -> Lens' (Network d) (Maybe Canceler)
_processCanceler inletPath =
    lens getter setter
    where
        cancelerLens = at inletPath
        getter (Network _ { processCancelers }) = view cancelerLens processCancelers
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { processCancelers = set cancelerLens val nwstate.processCancelers }


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
                , links : Set.empty
                }

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
        , process : identity
        }


addNode'
    :: forall d
     . PatchId
    -> NodeDef d
    -> Network d
    -> Rpd (Network d)
addNode' patchId def nw = do
    dataPFlow@(PushableFlow _ dataFlow) <- makePushableFlow
    let processFlow = makeProcessFlow dataFlow
    pure $ do
        nodePath <- nextNodePath patchId nw
        let
            newNode =
                Node
                    nodePath
                    def
                    { inlets : Set.empty
                    , outlets : Set.empty
                    , flow : dataPFlow
                    , processFlow : processFlow
                    }
        pure $ nw
             # setJust (_node nodePath) newNode
             # setJust (_patchNode patchId nodePath) unit


makeProcessFlow :: forall d. Flow (InletPath /\ d) -> Flow (InletPath /-> d)
makeProcessFlow dataFlow =
    E.fold (uncurry Map.insert) dataFlow Map.empty


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
    pushableFlow@(PushableFlow pushData dataFlow) <- makePushableFlow
    pure $ do
        (Node _ _ { flow }) :: Node d <- view (_node nodePath) nw # note (RpdError "")
        let (PushableFlow pushNodeData _ ) = flow
        inletPath <- nextInletPath nodePath nw
        let
            newInlet =
                Inlet
                    inletPath
                    def
                    { flow : pushableFlow
                    , connections : List.Nil
                    }
        -- TODO: iProcessCanceler :: Canceler <-
        --     liftEffect $ E.subscribe dataFlow (\d -> pushNodeData (inletPath /\ d))
        pure $ nw
             # setJust (_inlet inletPath) newInlet
             # setJust (_nodeInlet nodePath inletPath) unit
             -- # TODO: setJust (_processCanceler inletPath) iProcessCanceler


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
        }


addOutlet'
    :: forall d
     . NodePath
    -> OutletDef d
    -> Network d
    -> Rpd (Network d)
addOutlet' nodePath def nw = do
    pushableFlow <- makePushableFlow
    pure $ do
        outletPath <- nextOutletPath nodePath nw
        let
            newOutlet =
                Outlet
                    outletPath
                    def
                    { flow : pushableFlow
                    , connections : List.Nil
                    }
        pure $ nw
             # setJust (_outlet outletPath) newOutlet
             # setJust (_nodeOutlet nodePath outletPath) unit


connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
-- FIXME: rewrite for the case of different patches
connect outletPath inletPath
    nw@(Network nwdef nwstate) = do
    -- let patchId = extractPatchId outletPath inletPath
    let
        linkId = nextLinkId nw
        newLink = Link outletPath inletPath
        iNodePath = getNodeOfInlet inletPath
        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

        subscribeAndSave :: (PushableFlow d /\ PushableFlow d) -> Rpd (Network d)
        subscribeAndSave (outletPFlow /\ inletPFlow) = do
            let
                (PushableFlow _ outletFlow) = outletPFlow
                (PushableFlow pushToInlet inletFlow) = inletPFlow
                inletConnection = InletConnection outletPath linkId
                outletConnection = OutletConnection inletPath linkId
                curInletConnections = fold $ view (_inletConnections inletPath) nw
                curOutletConnections = fold $ view (_outletConnections outletPath) nw

            linkCanceler :: Canceler <-
                E.subscribe outletFlow pushToInlet

            network' :: Network d <-
                pure $ nw
                     # setJust (_link linkId) newLink
                     # setJust (_patchLink iPatchId linkId) unit
                     # setJust (_patchLink oPatchId linkId) unit
                     # setJust (_inletConnections inletPath)
                            (inletConnection : curInletConnections)
                     # setJust (_outletConnections outletPath)
                            (outletConnection : curOutletConnections)
                     # setJust (_canceler linkId) linkCanceler

            pure $ Right $ network'

    eFlows :: Either RpdError (PushableFlow d /\ PushableFlow d) <-
        pure $ (/\)
            <$> (view (_outletPFlow outletPath) nw # note (RpdError ""))
            <*> (view (_inletPFlow inletPath) nw # note (RpdError ""))

    either
        (const $ pure $ pure nw)
        identity
        $ subscribeAndSave <$> eFlows


disconnectAll
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
disconnectAll outletPath inletPath
    nw@(Network nwdef nwstate@{ links }) = do
    -- let patchId = extractPatchId outletPath inletPath
    let
        linkForDeletion (Link outletPath' inletPath') =
            (outletPath' == outletPath) && (inletPath' == inletPath)
        linksForDeletion = Map.keys $ links # Map.filter linkForDeletion

        iConnectionForDeletion (InletConnection outletPath' _) = outletPath' == outletPath
        oConnectionForDeletion (OutletConnection inletPath' _) = inletPath' == inletPath
        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

        curInletConnections = fold $ view (_inletConnections inletPath) nw
        curOutletConnections = fold $ view (_outletConnections outletPath) nw
        newInletConnections = curInletConnections # List.filter iConnectionForDeletion
        newOutletConnections = curOutletConnections # List.filter oConnectionForDeletion

    _ <- traverse_
            (\linkId -> fromMaybe (pure unit) $ view (_canceler linkId) nw)
            linksForDeletion

    pure $ Right $
        (
            foldr (\linkId nw ->
                nw # set (_link linkId) Nothing
                   # set (_patchLink iPatchId linkId) Nothing
                   # set (_patchLink oPatchId linkId) Nothing
                   # set (_canceler linkId) Nothing
            ) nw linksForDeletion
            # setJust (_inletConnections inletPath) newInletConnections
            # setJust (_outletConnections outletPath) newOutletConnections
            -- # note (RpdError "")
        )

    -- TODO: un-subscribe `process`` function of the target node to update values including this connection

-- TODO: disconnectTop

-- TODO: disconnectTopOf (OutletPath /\ InletPath)


sendToInlet
    :: forall d
     . InletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToInlet inletPath d nw = do
    performPush >>= alwaysNetwork
    where
        alwaysNetwork = const $ pure $ pure nw
        performPush = sequence
            $ view (_inletPFlow inletPath) nw # note (RpdError "")
                >>= \(PushableFlow push _) -> pure $ push d


streamToInlet
    :: forall d
     . InletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToInlet inletPath flow nw = do
    sequence
        $ view (_inletPFlow inletPath) nw # note (RpdError "")
            >>= \(PushableFlow push _) -> pure $ E.subscribe flow push


sendToOutlet
    :: forall d
     . OutletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToOutlet outletPath d nw = do
    performPush >>= alwaysNetwork
    where
        alwaysNetwork = const $ pure $ pure nw
        performPush = sequence
            $ view (_outletPFlow outletPath) nw # note (RpdError "")
                >>= \(PushableFlow push _) -> pure $ push d


streamToOutlet
    :: forall d
     . OutletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToOutlet outletPath flow nw = do
    sequence
        $ view (_outletPFlow outletPath) nw # note (RpdError "")
            >>= \(PushableFlow push _) -> pure $ E.subscribe flow push


subscribeInlet
    :: forall d
     . InletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeInlet inletPath handler nw =
    sequence subE
    where
        (flowE :: Either RpdError (Flow d)) =
            view (_inletFlow inletPath) nw # note (RpdError "")
        (subE :: Either RpdError Subscriber) =
            (handler # (flip $ E.subscribe)) <$> flowE

subscribeOutlet
    :: forall d
     . OutletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeOutlet outletPath handler nw =
    sequence subE
    where
        (flowE :: Either RpdError (Flow d)) =
            view (_outletFlow outletPath) nw # note (RpdError "")
        (subE :: Either RpdError Subscriber) =
            (handler # (flip $ E.subscribe)) <$> flowE
        -- MonadTrans lift :: m a -> ExceptT e m a


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


subscribeAllData
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect ((OutletPath /-> Canceler) /\ (InletPath /-> Canceler))
subscribeAllData oHandler iHandler nw =
    bisequence $ subscribeAllOutlets oHandler nw /\ subscribeAllInlets iHandler nw


isNodeInPatch :: NodePath -> PatchId -> Boolean
isNodeInPatch (NodePath patchId' _) patchId =
    patchId == patchId'


isInletInPatch :: InletPath -> PatchId -> Boolean
isInletInPatch (InletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isOutletInPatch :: OutletPath -> PatchId -> Boolean
isOutletInPatch (OutletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isInletInNode :: InletPath -> NodePath -> Boolean
isInletInNode (InletPath nodePath' _) nodePath =
    nodePath == nodePath'


isOutletInNode :: OutletPath -> NodePath -> Boolean
isOutletInNode (OutletPath nodePath' _) nodePath =
    nodePath == nodePath'


notInTheSameNode :: InletPath -> OutletPath -> Boolean
notInTheSameNode (InletPath iNodePath _) (OutletPath oNodePath _) =
    iNodePath /= oNodePath


updatePatch
    :: forall d
     . (Patch d -> Patch d)
    -> PatchId
    -> Network d
    -> RpdOp (Network d)
updatePatch updater patchId nw@(Network def state@{ patches }) = do
    patch <- Map.lookup patchId patches # note (RpdError "")
    pure $
        Network
            def
            state
                { patches =
                    Map.insert patchId (updater patch) patches
                }


updateNode
    :: forall d
     . (Node d -> Node d)
    -> NodePath
    -> Network d
    -> RpdOp (Network d)
updateNode updater path@(NodePath patchId _) (Network def state@{ nodes }) = do
    node <- Map.lookup path nodes # note (RpdError "")
    pure $
        Network
            def
            state
                { nodes =
                    Map.insert path (updater node) nodes
                }

updateInlet
    :: forall d
     . (Inlet d -> Inlet d)
    -> InletPath
    -> Network d
    -> RpdOp (Network d)
updateInlet updater path@(InletPath nodePath _) (Network def state@{ inlets }) = do
    inlet <- Map.lookup path inlets # note (RpdError "")
    let inlets' = Map.insert path inlet inlets
    pure $
        Network
            def
            state { inlets = inlets' }


updateOutlet
    :: forall d
     . (Outlet d -> Outlet d)
    -> OutletPath
    -> Network d
    -> RpdOp (Network d)
updateOutlet updater path@(OutletPath nodePath _) (Network def state@{ outlets }) = do
    outlet <- Map.lookup path outlets # note (RpdError "")
    let outlets' = Map.insert path outlet outlets
    pure $
        Network
            def
            state { outlets = outlets' }


patchId :: Int -> PatchId
patchId = PatchId


nodePath :: Int -> Int -> NodePath
nodePath pId nId = NodePath (PatchId pId) nId


inletPath :: Int -> Int -> Int -> InletPath
inletPath pId nId iId = InletPath (NodePath (PatchId pId) nId) iId


outletPath :: Int -> Int -> Int -> OutletPath
outletPath pId nId iId = OutletPath (NodePath (PatchId pId) nId) iId


-- FIXME: below are Lenses/Prisms

getPatchOfNode :: NodePath -> PatchId
getPatchOfNode (NodePath pId _) = pId


getPatchOfInlet :: InletPath -> PatchId
getPatchOfInlet inlet = getPatchOfNode $ getNodeOfInlet inlet


getPatchOfOutlet :: OutletPath -> PatchId
getPatchOfOutlet outlet = getPatchOfNode $ getNodeOfOutlet outlet


getNodeOfInlet :: InletPath -> NodePath
getNodeOfInlet  (InletPath nPath _) = nPath


getNodeOfOutlet :: OutletPath -> NodePath
getNodeOfOutlet  (OutletPath nPath _) = nPath


unpackNodePath :: NodePath -> Array Int
unpackNodePath (NodePath (PatchId patchId) id) = [ patchId, id ]

unpackInletPath :: InletPath -> Array Int
unpackInletPath (InletPath nodePath id) = unpackNodePath nodePath <> [ id ]

unpackOutletPath :: OutletPath -> Array Int
unpackOutletPath (OutletPath nodePath id) = unpackNodePath nodePath <> [ id ]


instance showRpdError :: Show RpdError where
    show (RpdError text) = "Error: " <> text


instance showPatchId :: Show PatchId where
    show (PatchId id) = "P" <> show id

instance showNodePath :: Show NodePath where
    show (NodePath patchId id) = show patchId <> "/N" <> show id

instance showInletPath :: Show InletPath where
    show (InletPath nodePath id) = show nodePath <> "/I" <> show id

instance showOutletPath :: Show OutletPath where
    show (OutletPath nodePath id) = show nodePath <> "/O" <> show id

instance showLinkId :: Show LinkId where
    show (LinkId id) = "L" <> show id

instance showLink :: Show Link where
    show (Link outletPath inletPath) = "Link " <> show outletPath <> " -> " <> show inletPath


instance eqPatchId :: Eq PatchId where
    eq (PatchId a) (PatchId b) = a == b

instance eqNodePath :: Eq NodePath where
    eq (NodePath pa a) (NodePath pb b) = (pa == pb) && (a == b)

instance eqInletPath :: Eq InletPath where
    eq (InletPath na a) (InletPath nb b) = (na == nb) && (a == b)

instance eqOutletPath :: Eq OutletPath where
    eq (OutletPath na a) (OutletPath nb b) = (na == nb) && (a == b)

instance eqLinkId :: Eq LinkId where
    eq (LinkId a) (LinkId b) = a == b

-- instance eqDataSource :: Eq (DataSource d) where
--     eq (OutletSource oa a) (OutletSource ob b) = oa == ob
--     eq _ _ = false


instance ordPatchId :: Ord PatchId where
    compare (PatchId a) (PatchId b) = compare a b

instance ordNodePath :: Ord NodePath where
    compare nodePath1 nodePath2 =
        compare (unpackNodePath nodePath1)  (unpackNodePath nodePath2)

instance ordInletPath :: Ord InletPath where
    compare inletPath1 inletPath2 =
        compare (unpackInletPath inletPath1)  (unpackInletPath inletPath2)

instance ordOutletPath :: Ord OutletPath where
    compare outletPath1 outletPath2 =
        compare (unpackOutletPath outletPath1) (unpackOutletPath outletPath2)

instance ordLinkId :: Ord LinkId where
    compare (LinkId a) (LinkId b) =
        compare a b

-- instance ordDataSource :: Ord (DataSource d) where
--     compare (OutletSource oa a) (OutletSource ob b) = compare oa ob
--     compare _ _ = LT


-- TODO: create HasId / HasPath typeclass
instance eqPatch :: Eq (Patch d) where
    eq (Patch idA _ _) (Patch idB _ _) = (idA == idB)

instance eqNode :: Eq (Node d) where
    eq (Node pathA _ _) (Node pathB _ _) = (pathA == pathB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet pathA _ _) (Inlet pathB _ _) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet pathA _ _) (Outlet pathB _ _) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq (Link outletA inletA) (Link outletB inletB) = (outletA == outletB) && (inletA == inletB)
