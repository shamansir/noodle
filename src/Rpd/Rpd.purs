module Rpd
    ( Rpd, RpdError, init
    , (</>), type (/->), rpdAp, run, emptyNetwork
    --, RpdOp, RpdEffOp
    , DataSource(..), Flow, getFlowOf, flow
    , Network, Patch, Node, Inlet, Outlet, Link
    , PatchDef, NodeDef, InletDef, OutletDef
    , Canceler, Subscriber, PushableFlow
    --, emptyNetwork
    --, network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    --, connect, connect', disconnect, disconnect', disconnectTop
    , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    , subscribeInlet, {- subscribeOutlet, -} subscribeAllData, subscribeAllInlets, subscribeAllOutlets
    , sendToInlet, streamToInlet
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

--import Control.Monad.Cont.Trans (ContT(..))
-- import Control.MonadZero (guard)
-- import Control.Alternative ((<|>))

import Data.Either (Either(..), either, note)
import Data.Lens (Lens', Getter', lens, view, set, setJust, over, to)
import Data.Lens.At (at)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Data.Bitraversable (bisequence)
import Data.Tuple.Nested ((/\), type (/\))

import FRP.Event (Event, subscribe, create)


--import Rpd.Flow as Flow

-- data RunningNetwork d e = RpdEff e (Network d e)

data RpdError = RpdError String


type RpdOp a = Either RpdError a
--type RpdEffOp e a = RpdEff e (RpdOp e a)
type Rpd a = Effect (Either RpdError a)
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


infixl 1 rpdAp as </> -- FIXME: can be replaced with proper instances?
-- other options: └, ~>, ...


run
    :: forall d
     . (RpdError -> Effect Unit)
    -> (Network d -> Effect Unit)
    -> Rpd (Network d)
    -> Effect Unit
run onError onSuccess rpd =
    rpd >>= either onError onSuccess


-- FIXME: continuation monad
-- (a -> r) -> r
-- (a -> m r) -> m r
-- (a -> Eff (Except err a)) -> Eff (Except err a)
-- ContT (Except err a) Eff a
rpdAp :: forall a. Rpd a -> (a -> Rpd a) -> Rpd a
rpdAp eff f =
    eff >>= either (pure <<< Left) f


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


type Flow d = Event d
type PushF d = (d -> Effect Unit)
data PushableFlow d = PushableFlow (PushF d) (Flow d)

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


data DataSource d
    = UserSource (Flow d)
    | OutletSource OutletPath (Flow d)
    -- | UISource (Flow d)

type PatchDef d =
    { name :: String
    , nodeDefs :: List (NodeDef d)
    -- , linkDefs :: Array LinkDef -- TODO: links partly duplicate Inlet: sources, aren't they?
    -- TODO: maybe store Connections: Map InletPath (Array DataSource)
    }
type NodeDef d =
    { name :: String
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: ProcessF d
    -- , flow :: Flow (Map (Inlet d) d /\ Map (Outlet d) d)
    }
type InletDef d =
    { label :: String
    , default :: Maybe d
    , accept :: Maybe d
    -- , sources :: Array (DataSource d)
    -- Maybe (AdaptF d)
    }
type OutletDef d =
    { label :: String
    -- , flow :: Maybe (Flow d)
    }
-- type LinkDef = (Int /\ Int) /\ (Int /\ Int)

infixr 6 type Map as /->

-- TODO: normalize network, change to plain IDs maybe, or use paths as keys,
--       they implement Eq anyway
-- TODO: try to get rid of `e` by using `forall e.` where possible (`PushableFlow` has to have
--       it in context of one call)
data Network d =
    Network -- (NetworkDef d)
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: PatchId /-> Patch d
        , nodes :: NodePath /-> Node d
        , inlets :: InletPath /-> Inlet d
        , outlets :: OutletPath /-> Outlet d
        , links :: LinkId /-> Link
        , linkCancelers :: LinkId /-> Canceler
        }
data Patch d =
    Patch
        PatchId
        (PatchDef d)
        { nodes :: Set NodePath -- FIXME: all Lists should be sets
        , links :: Set LinkId -- TODO: links partly duplicate Inlet: sources, aren't they?
        -- TODO: maybe store Connections: Map InletPath (Array DataSource)
        }
data Node d =
    Node
        NodePath -- (NodeDef d)
        (NodeDef d)
        { inlets :: Set InletPath
        , outlets :: Set OutletPath
        }
-- S.constant is not able to send values afterwards, so we store the default value inside
-- TODO: inlet sources should be a set of outletPaths, so outlet-inlet pairs would be unique
data Inlet d =
    Inlet
        InletPath
        (InletDef d)
        { sources :: Set (DataSource d)
        -- , accept :: d
        -- Maybe (AdaptF d)
        , flow :: PushableFlow d
        }

--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d =
    Outlet
        OutletPath
        (OutletDef d)
        { flow :: PushableFlow d
        }
data Link = Link OutletPath InletPath


init :: forall d. String -> Rpd (Network d)
init = pure <<< pure <<< emptyNetwork


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


_inletSource :: forall d. InletPath -> DataSource d -> Lens' (Network d) (Maybe Unit)
_inletSource inletPath source =
    lens getter setter
    where
        inletLens = _inlet inletPath
        sourceLens = at source
        getter nw =
            view inletLens nw
            >>= \(Inlet _ _ { sources }) -> view sourceLens sources
        setter nw val =
            over inletLens
                (map $ \(Inlet iid idef istate) ->
                    Inlet
                        iid
                        idef
                        istate { sources = set sourceLens val istate.sources }
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



-- data NormalizedNetwork d =
--     NormalizedNetwork
--         (Array (Patch' d))
--         (Array (Node' d))
--         (Array (Inlet' d))
--         (Array (Outlet' d))
--         (Array (Link' d))

-- type WithId e a = Eff ( random :: RANDOM | e ) a


-- data DataMsg d
--     = FromInlet InletPath d
--     | FromOutlet OutletPath d

-- may be it will make more sense when we'll do subscriptions before passing network to rederer
-- also, may be change to ehm... `UnpreparedNetwork`` and then the subscriber gets the `Real` one?
-- is it the place where RPD effect should be added to the result, like with subscriptions?
-- run :: forall d e. (Rpd d e -> RpdEff e Unit) -> Rpd d e -> RpdEff e Unit
-- run renderer nw = renderer nw


makePushableFlow :: forall d e. Effect (PushableFlow d)
makePushableFlow = do
    { push, event } <- create
    pure $ PushableFlow push event

{-
emptyNetwork :: forall d. Network d
emptyNetwork = Network { patches : [] }


emptyPatch :: forall d. PatchId -> Patch d
emptyPatch id =
    Patch id
        { name : }
        { patches : [] }
-}


-- network :: forall d. Array (Patch d) -> Network d
-- network lazyPatches =
--     Network { patches }
--     where
--         patches = mapWithIndex
--             (\idx lazyPatch -> lazyPatch $ PatchId idx) lazyPatches


-- TODO: use lenses below

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
    pure $ pure $ setJust (_patch patchId) newPatch nw
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
addNode' patchId def nw =
    pure $ do
        nodePath <- nextNodePath patchId nw
        let
            newNode =
                Node
                    nodePath
                    def
                    { inlets : Set.empty, outlets : Set.empty }
        pure $ nw
             # setJust (_node nodePath) newNode
             # setJust (_patchNode patchId nodePath) unit


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
    pushableFlow <- makePushableFlow
    pure $ do
        inletPath <- nextInletPath nodePath nw
        let
            newInlet =
                Inlet
                    inletPath
                    def
                    { sources : Set.empty
                    , flow : pushableFlow
                    }
        pure $ nw
             # setJust (_inlet inletPath) newInlet
             # setJust (_nodeInlet nodePath inletPath) unit


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
                    { flow : pushableFlow }
        pure $ nw
             # setJust (_outlet outletPath) newOutlet
             # setJust (_nodeOutlet nodePath outletPath) unit


guardE :: forall a. a -> Boolean -> String -> Either RpdError a
guardE v check errorText =
    if check then pure v else Left (RpdError errorText)


connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
connect outletPath inletPath
    nw@(Network nwdef nwstate@{ nodes, outlets, inlets, links }) = do
    -- let patchId = extractPatchId outletPath inletPath
    let linkId = nextLinkId nw
    let newLink = Link outletPath inletPath

    ePatchId :: Either RpdError PatchId <-
        pure $ extractPatchId outletPath inletPath
    eFlows :: Either RpdError (PushableFlow d /\ PushableFlow d) <-
        pure $ (/\)
            <$> (view (_outletPFlow outletPath) nw # note (RpdError ""))
            <*> (view (_inletPFlow inletPath) nw # note (RpdError ""))

    let
        subscribeAndSave :: PatchId -> (PushableFlow d /\ PushableFlow d) -> Rpd (Network d)
        subscribeAndSave patchId (outletPFlow /\ inletPFlow) = do
            let (PushableFlow _ outletFlow) = outletPFlow
            let (PushableFlow pushToInlet inletFlow) = inletPFlow
            let newSource = OutletSource outletPath outletFlow

            canceler :: Canceler <- subscribe outletFlow pushToInlet

            network' :: Network d <-
                pure $ nw
                     # setJust (_link linkId) newLink
                     # setJust (_patchLink patchId linkId) unit
                     # setJust (_inletSource inletPath newSource) unit
                     -- # note (RpdError "")
                    -- TODO: store canceler
                    -- TODO: re-subscribe `process`` function of the target node to update values including this connection

            pure $ Right $ network'

    either (const $ pure $ pure nw) identity
        $ subscribeAndSave <$> ePatchId <*> eFlows

    where
        extractPatchId :: OutletPath -> InletPath -> Either RpdError PatchId
        extractPatchId outletPath inletPath =
            let
                outletPatch = getPatchOfOutlet outletPath
                inletPatch = getPatchOfInlet inletPath
            in
                guardE inletPatch (inletPatch == outletPatch) ""


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
            >>= \(PushableFlow push _) -> pure $ subscribe flow push


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
            (handler # (flip $ subscribe)) <$> flowE


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
                PushableFlow _ fl -> subscribe fl $ handler inletPath


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
                PushableFlow _ fl -> subscribe fl $ handler outletPath


subscribeAllData
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect ((OutletPath /-> Canceler) /\ (InletPath /-> Canceler))
subscribeAllData oHandler iHandler nw =
    bisequence $ subscribeAllOutlets oHandler nw /\ subscribeAllInlets iHandler nw


{-
node :: forall d. String -> Array (Inlet d) -> Array (Outlet d) -> Node d
node name lazyInlets lazyOutlets =
    \nodePath ->
        let
            inlets = mapWithIndex
                (\idx lazyInlet ->
                    lazyInlet (InletPath nodePath idx)) lazyInlets
            outlets = mapWithIndex
                (\idx lazyOutlet ->
                    lazyOutlet (OutletPath nodePath idx)) lazyOutlets
        in
            Node
                { path : nodePath
                , name
                , inlets
                , outlets
                , process : const Map.empty
                }

inlet :: forall d. String -> Inlet d
inlet label =
    inlet_ label Nothing []


inlet' :: forall d. String -> Flow d -> Inlet d
inlet' label dataSource =
    inlet_ label Nothing [ UserSource dataSource ]


-- should not be exposed
inlet_ :: forall d. String -> Maybe d -> Array (DataSource d) -> Inlet d
inlet_ label maybeDefault sources =
    \inletPath ->
        Inlet
            { path : inletPath
            , label
            , default : maybeDefault
            , sources
            }


inletWithDefault :: forall d. String -> d -> Inlet d
inletWithDefault label defaultVal =
    -- inlet_ label defaultVal $ S.constant defaultVal
    inlet_ label (Just defaultVal) [ ]


inletWithDefault' :: forall d. String -> d -> Flow d -> Inlet d
inletWithDefault' label defaultVal dataSource  =
    inlet_ label (Just defaultVal) [ UserSource dataSource ]


outlet :: forall d. String -> Outlet d
outlet label =
    \outletPath ->
        Outlet
            { path : outletPath
            , label
            , flow : Nothing
            }


-- TODO: remove, outlets should only produce values from `process` function
outlet' :: forall d. String -> Flow d -> Outlet d
outlet' label flow =
    \outletPath ->
        Outlet
            { path : outletPath
            , label
            , flow : Just flow
            }

-}

{-
findTopConnection :: forall d. InletPath -> Network d -> Maybe OutletPath
findTopConnection inletPath network =
    findInlet inletPath network >>=
        (\(Inlet { sources }) -> findMap (\src ->
                case src of
                    UserSource _ -> Nothing
                    OutletSource outletPath _ -> Just outletPath
            ) sources
        )

findTopSource :: forall d. InletPath -> Network d -> Maybe (DataSource d)
findTopSource inletPath network =
    findInlet inletPath network >>= \(Inlet { sources }) -> head sources


findSource :: forall d. OutletPath -> InletPath -> Network d -> Maybe (DataSource d)
findSource outletPath inletPath network =
    findInlet inletPath network >>=
        (\(Inlet { sources }) -> findMap (\src ->
                -- OutletSource outletPath' _ <- src
                -- pure $ if (outletPath' == outletPath) then src else unit
                case src of
                    UserSource _ -> Nothing
                    OutletSource outletPath' _ ->
                        if (outletPath' == outletPath) then Just src
                        else Nothing
            ) sources
        )
-}


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


{-
findPatch :: forall d. PatchId -> Network d -> Maybe (Patch d)
findPatch (PatchId index) (Network { patches }) =
    patches !! index


findNode :: forall d. NodePath -> Network d -> Maybe (Node d)
findNode (NodePath patchId index) network =
    findPatch patchId network >>= (\(Patch { nodes }) -> nodes !! index)


findInlet :: forall d. InletPath -> Network d -> Maybe (Inlet d)
findInlet (InletPath nodePath index) network =
    findNode nodePath network >>= (\(Node { inlets }) -> inlets !! index)


findOutlet :: forall d. OutletPath -> Network d -> Maybe (Outlet d)
findOutlet (OutletPath nodePath index) network =
    findNode nodePath network >>= (\(Node { outlets }) -> outlets !! index)
-}

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


-- getInletLabel :: forall d. Node d -> InletPath -> Maybe String
-- getInletLabel (Node { inlets }) path' =
--     Array.filter (\(Inlet { path }) -> path == path') path inlets <#> \(Inlet { label }) -> label


-- processWith' :: forall d. ProcessF' d -> LazyNode d -> LazyNode d
-- processWith' processF =
--     processWith f
--     where
--         f inputs = Map.mapWithKey processF inputs


-- processWith'' :: forall d. ProcessF'' d -> d -> LazyNode d -> LazyNode d
-- processWith'' processF default =
--     processWith f
--     where
--         f inputs = map (fromMaybe default)
--                     $ Map.filter isJust
--                     $ Map.mapWithKey processF inputs


-- processWith''' :: forall d. ProcessF''' d -> d -> LazyNode d -> LazyNode d
-- processWith''' processF default =
--     processWith f
--     where
--         f inputs = map (fromMaybe default)
--                     $ Map.filter isJust
--                     $ Map.mapWithKey processF inputs


{-
connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Patch d
    -> Patch d
connect outletPath inletPath patchF =
    -- TODO: implement
    patchF
-}

{-
connect' :: forall d. OutletPath -> InletPath -> Network d -> Maybe (Network d)
connect' outletPath inletPath network = do
    outlet <- findOutlet outletPath network
    let (Outlet { flow }) = outlet
    flow' <- flow
    let
        -- flow' = fromMaybe' (\_ -> ) flow
        patchId = getPatchOfInlet inletPath
        newLink = Link outletPath inletPath
        network' = updatePatch
                    (\(Patch patch@{ links }) -> Patch patch { links = newLink : links })
                    patchId
                    network
        newSource = OutletSource outletPath flow'
    pure $ updateInlet
                (\(Inlet inlet@{ sources }) ->
                    Inlet inlet { sources = newSource : sources })
                inletPath
                network'
-}


disconnect :: forall d. Outlet d -> Inlet d -> Patch d -> Patch d
disconnect outlet inlet patch =
    patch -- FIXME: implement

{-
disconnect' :: forall d. OutletPath -> InletPath -> Network d -> Maybe (Network d)
disconnect' outletPath inletPath network = do
    network' <- removeLink outletPath inletPath network
    outlet <- findOutlet outletPath network'
    inlet <- findInlet inletPath network'
    -- source <- findSource outletPath inletPath network'
    let
        isFromOutlet oPath1 (OutletSource oPath2 _) = oPath1 == oPath2
        isFromOutlet _ _ = false
    -- TODO: optimize, do not search through network several times
    pure $ updateInlet
                (\(Inlet inlet@{ sources }) ->
                    -- Inlet inlet { sources = deleteBy cmpSources source sources })
                    Inlet inlet { sources = filter (not $ isFromOutlet outletPath) sources })
                inletPath
                network'
-}

{-
disconnectTop :: forall d. InletPath -> Network d -> Maybe (Network d)
disconnectTop inletPath network =
    -- TODO: optimize with searching last and updating simultaneously
    findTopConnection inletPath network
        >>= (\outletPath -> disconnect' outletPath inletPath network)
-}

-- disconnectLast' :: forall d. InletPath -> Network d -> Maybe (Network d)

-- getConnections :: Node -> (Map OutletPath InletPath) or (Array Link)

-- getConnections :: Patch -> (Map OutletPath InletPath) or (Array Link)

-- TODO: findLink :: forall d. InletPath -> Network d -> Maybe (Link d)

{-
removeLink :: forall d. OutletPath -> InletPath -> Network d -> Maybe (Network d)
removeLink outletPath inletPath network = do
    let
        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath
    patchId <- if oPatchId == iPatchId then Just oPatchId else Nothing
    let
        network' = updatePatch
                    (\(Patch patch@{ links }) ->
                        Patch patch { links = delete (Link outletPath inletPath) links })
                    patchId
                    network
    pure network'
-}


getFlowOf :: forall d. DataSource d -> Flow d
getFlowOf dataSource =
    case dataSource of
        UserSource flow -> flow
        OutletSource _ flow -> flow


patchId :: Int -> PatchId
patchId = PatchId


nodePath :: Int -> Int -> NodePath
nodePath pId nId = NodePath (PatchId pId) nId


inletPath :: Int -> Int -> Int -> InletPath
inletPath pId nId iId = InletPath (NodePath (PatchId pId) nId) iId


outletPath :: Int -> Int -> Int -> OutletPath
outletPath pId nId iId = OutletPath (NodePath (PatchId pId) nId) iId


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


-- connect inside a Patch??
-- connect :: forall d e. Inlet d -> Outlet d -> d -> Eff ( channel :: SC.CHANNEL | e ) (SC.Channel d)
-- connect inlet outlet defaultVal = do
--     channel <- SC.channel defaultVal
--     pure channel


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

instance eqDataSource :: Eq (DataSource d) where
    eq (OutletSource oa a) (OutletSource ob b) = oa == ob
    eq _ _ = false


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

instance ordDataSource :: Ord (DataSource d) where
    compare (OutletSource oa a) (OutletSource ob b) = compare oa ob
    compare _ _ = LT


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
