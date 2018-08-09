module Rpd
    ( Rpd, RpdEff, RpdEffE, RpdError, init
    , (~>), type (/->), rpdAp, run, emptyNetwork
    --, RpdOp, RpdEffOp
    , DataSource(..), Flow, getFlowOf
    , Network, Patch, Node, Inlet, Outlet, Link
    , PatchDef, NodeDef, InletDef, OutletDef
    , Canceler, Subscriber, PushableFlow
    --, emptyNetwork
    --, network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    --, connect, connect', disconnect, disconnect', disconnectTop
    , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    , subscribeInlet, {- subscribeOutlet, -} subscribeAllData, subscribeAllInlets, subscribeAllOutlets
    , ProcessF
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    --, findPatch, findNode, findOutlet, findInlet
    ) where

import Data.Either
import Prelude
import Unsafe.Coerce

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadZero (guard)
import Data.Foldable (foldMap)
import Data.Lens (Lens', Getter', lens, view, set, setJust, over, to)
import Data.Lens.At (at)
import Data.List (List(..), (:), (!!), mapWithIndex, modifyAt, findMap, delete, filter, head, length)
import Data.List as List
import Data.Map (Map, mapWithKey)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Data.Bitraversable (bisequence)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import FRP (FRP)
import FRP.Event (Event, subscribe, create)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)


--import Rpd.Flow as Flow

-- foreign import data RPD :: Effect

-- FIXME: !!! remove REF and CONSOLE
type RpdEffE e = ( frp :: FRP, ref :: REF, console :: CONSOLE | e )
type RpdEff e v = Eff (RpdEffE e) v

-- data RunningNetwork d e = RpdEff e (Network d e)

data RpdError = RpdError String


type RpdOp d e = Either RpdError (Network d e)
type RpdEffOp d e = RpdEff e (RpdOp d e)
type Rpd d e = RpdEff e (Either RpdError (Network d e))
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


infixl 1 rpdAp as ~> -- FIXME: can be replaced with proper instances?


run :: forall d e. (RpdError -> RpdEff e Unit) -> (Network d e -> RpdEff e Unit) -> Rpd d e -> RpdEff e Unit
run onError onSuccess rpd =
    rpd >>= either onError onSuccess


rpdAp :: forall d e. Rpd d e -> (Network d e -> Rpd d e) -> Rpd d e
rpdAp eff f =
    eff >>= either (pure <<< Left) f


someApiFunc :: forall d e. Rpd d e
someApiFunc =
    init "t"
        ~> addPatch "foo"
        ~> addNode (PatchId 0) "test1"
        ~> addNode (PatchId 0) "test2"


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


type Flow d = Event d
type PushF d e = (d -> RpdEff e Unit)
data PushableFlow d e = PushableFlow (PushF d e) (Flow d)

type Canceler e =
    RpdEff e Unit
type Subscriber e =
    RpdEff e (Canceler e)


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
data Network d e =
    Network -- (NetworkDef d)
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: PatchId /-> Patch d
        , nodes :: NodePath /-> Node d
        , inlets :: InletPath /-> Inlet d e
        , outlets :: OutletPath /-> Outlet d e
        , links :: LinkId /-> Link
        , linkCancelers :: LinkId /-> Canceler e
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
data Inlet d e =
    Inlet
        InletPath
        (InletDef d)
        { sources :: Set (DataSource d)
        -- , accept :: d
        -- Maybe (AdaptF d)
        , flow :: PushableFlow d e
        }

--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d e =
    Outlet
        OutletPath
        (OutletDef d)
        { flow :: PushableFlow d e
        }
data Link = Link OutletPath InletPath


init :: forall d e. String -> Rpd d e
init = pure <<< pure <<< emptyNetwork


emptyNetwork :: forall d e. String -> Network d e
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



_patch :: forall d e. PatchId -> Lens' (Network d e) (Maybe (Patch d))
_patch patchId =
    lens getter setter
    where
        patchLens = at patchId
        getter (Network _ { patches }) = view patchLens patches
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { patches = set patchLens val nwstate.patches }


_patchNode :: forall d e. PatchId -> NodePath -> Lens' (Network d e) (Maybe Unit)
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


_node :: forall d e. NodePath -> Lens' (Network d e) (Maybe (Node d))
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


_nodeInlet :: forall d e. NodePath -> InletPath -> Lens' (Network d e) (Maybe Unit)
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


_inlet :: forall d e. InletPath -> Lens' (Network d e) (Maybe (Inlet d e))
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


_inletFlow :: forall d e. InletPath -> Getter' (Network d e) (Maybe (Flow d))
_inletFlow inletPath =
    to extractFlow
    where
        inletLens = _inlet inletPath
        extractFlow nw = view inletLens nw >>=
            (\(Inlet _ _ { flow }) ->
                case flow of
                    (PushableFlow _ fl) -> Just fl)


_inletSource :: forall d e. InletPath -> DataSource d -> Lens' (Network d e) (Maybe Unit)
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


_nodeOutlet :: forall d e. NodePath -> OutletPath -> Lens' (Network d e) (Maybe Unit)
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


_outlet :: forall d e. OutletPath -> Lens' (Network d e) (Maybe (Outlet d e))
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


_patchLink :: forall d e. PatchId -> LinkId -> Lens' (Network d e) (Maybe Unit)
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


_link :: forall d e. LinkId -> Lens' (Network d e) (Maybe Link)
_link linkId =
    lens getter setter
    where
        linkLens = at linkId
        getter (Network _ { links }) = view linkLens links
        setter nw@(Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { links = set linkLens val nwstate.links }


_canceler :: forall d e. LinkId -> Lens' (Network d e) (Maybe (Canceler e))
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


makePushableFlow :: forall d e. RpdEff e (PushableFlow d e)
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

nextPatchId :: forall d e. Network d e -> PatchId
nextPatchId (Network _ { patches }) =
    PatchId (Map.size patches)


nextNodePath :: forall d e. PatchId -> Network d e -> Either RpdError NodePath
nextNodePath patchId (Network _ { patches }) = do
    (Patch _ _ { nodes }) <- Map.lookup patchId patches
                                # note (RpdError "")
    pure $ NodePath patchId $ Set.size nodes


nextInletPath :: forall d e. NodePath -> Network d e -> Either RpdError InletPath
nextInletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { inlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ InletPath nodePath $ Set.size inlets


nextOutletPath :: forall d e. NodePath -> Network d e -> Either RpdError OutletPath
nextOutletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { outlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ OutletPath nodePath $ Set.size outlets


nextLinkId :: forall d e. Network d e -> LinkId
nextLinkId (Network _ { links }) =
    LinkId (Map.size links)


addPatch :: forall d e. String -> Network d e -> Rpd d e
addPatch name =
    addPatch'
        { name
        , nodeDefs : List.Nil
        }


addPatch' :: forall d e. PatchDef d -> Network d e -> Rpd d e
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
    :: forall d e
     . PatchId
    -> String
    -> Network d e
    -> Rpd d e
addNode patchId name =
    addNode'
        patchId
        { name
        , inletDefs : List.Nil
        , outletDefs : List.Nil
        , process : id
        }


addNode'
    :: forall d e
     . PatchId
    -> NodeDef d
    -> Network d e
    -> Rpd d e
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
    :: forall d e
     . NodePath
    -> String
    -> Network d e
    -> Rpd d e
addInlet nodePath label =
    addInlet'
        nodePath
        { label
        , default : Nothing
        , accept : Nothing
        }


addInlet'
    :: forall d e
     . NodePath
    -> InletDef d
    -> Network d e
    -> Rpd d e
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
    :: forall d e
     . NodePath
    -> String
    -> Network d e
    -> Rpd d e
addOutlet nodePath label =
    addOutlet'
        nodePath
        { label
        }


addOutlet'
    :: forall d e
     . NodePath
    -> OutletDef d
    -> Network d e
    -> Rpd d e
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
    :: forall d e
     . OutletPath
    -> InletPath
    -> Network d e
    -> Rpd d e
connect outletPath inletPath
    network@(Network nwdef nwstate@{ nodes, outlets, inlets, links }) = do
    -- let patchId = extractPatchId outletPath inletPath
    let linkId = nextLinkId network
    let newLink = Link outletPath inletPath

    ePatchId :: Either RpdError PatchId <-
        pure $ extractPatchId outletPath inletPath
    eFlows :: Either RpdError (PushableFlow d e /\ PushableFlow d e) <-
        pure $ extractFlows -- + TODO: `curry`` or do not return a tuple
            <$> (Map.lookup outletPath outlets # note (RpdError ""))
            <*> (Map.lookup inletPath  inlets  # note (RpdError ""))

    let
        subscribeAndSave :: PatchId -> (PushableFlow d e /\ PushableFlow d e) -> RpdEff e (Either RpdError (Network d e))
        subscribeAndSave patchId (outletPFlow /\ inletPFlow) = do
            let (PushableFlow _ outletFlow) = outletPFlow
            let (PushableFlow pushToInlet inletFlow) = inletPFlow
            let newSource = OutletSource outletPath outletFlow

            canceler :: Canceler e <- subscribe outletFlow pushToInlet

            network' :: Network d e <-
                pure $ network
                     # setJust (_link linkId) newLink
                     # setJust (_patchLink patchId linkId) unit
                     # setJust (_inletSource inletPath newSource) unit
                     -- # note (RpdError "")
                    -- TODO: store canceler
                    -- TODO: re-subscribe `process`` function of the target node to update values including this connection

            pure $ Right $ network'

        --(network' :: _) = subscribeAndSave <$> ePatchId <*> eFlows

    either (const $ pure $ pure network) id $ subscribeAndSave <$> ePatchId <*> eFlows

    -- subscribeAndSave <$> ePatchId <*> eFlows

    where
        extractFlows :: Outlet d e -> Inlet d e -> (PushableFlow d e /\ PushableFlow d e)
        extractFlows (Outlet _ _ { flow : outletPFlow }) (Inlet _ _ { flow : inletPFlow }) =
            outletPFlow /\ inletPFlow
        extractPatchId :: OutletPath -> InletPath -> Either RpdError PatchId
        extractPatchId outletPath inletPath =
            let
                outletPatch = getPatchOfOutlet outletPath
                inletPatch = getPatchOfInlet inletPath
            in
                guardE inletPatch (inletPatch == outletPatch) ""


subscribeInlet
    :: forall d e
     . InletPath
    -> (d -> RpdEff e Unit)
    -> Network d e
    -> RpdEff e (Either RpdError (Canceler e))
subscribeInlet inletPath handler nw =
    sequence subE
    where
        (flowE :: Either RpdError (Flow d)) =
            view (_inletFlow inletPath) nw # note (RpdError "")
        (subE :: Either RpdError (Subscriber e)) =
            (handler # (flip $ subscribe)) <$> flowE


subscribeAllInlets
    :: forall d e
     . (InletPath -> d -> RpdEff e Unit)
    -> Network d e
    -> RpdEff e (InletPath /-> Canceler e)
subscribeAllInlets handler (Network _ { inlets }) =
    traverse sub inlets
    where
        sub :: Inlet d e -> Subscriber e
        sub (Inlet inletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> subscribe fl $ handler inletPath


subscribeAllOutlets
    :: forall d e
     . (OutletPath -> d -> RpdEff e Unit)
    -> Network d e
    -> RpdEff e (OutletPath /-> Canceler e)
subscribeAllOutlets handler (Network _ { outlets }) =
    traverse sub outlets
    where
        sub :: Outlet d e -> Subscriber e
        sub (Outlet outletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> subscribe fl $ handler outletPath


subscribeAllData
    :: forall d e
     . (OutletPath -> d -> RpdEff e Unit)
    -> (InletPath -> d -> RpdEff e Unit)
    -> Network d e
    -> RpdEff e ((OutletPath /-> Canceler e) /\ (InletPath /-> Canceler e))
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
    :: forall d e
     . (Patch d -> Patch d)
    -> PatchId
    -> Network d e
    -> RpdOp d e
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
    :: forall d e
     . (Node d -> Node d)
    -> NodePath
    -> Network d e
    -> RpdOp d e
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
    :: forall d e
     . (Inlet d e -> Inlet d e)
    -> InletPath
    -> Network d e
    -> RpdOp d e
updateInlet updater path@(InletPath nodePath _) (Network def state@{ inlets }) = do
    inlet <- Map.lookup path inlets # note (RpdError "")
    let inlets' = Map.insert path inlet inlets
    pure $
        Network
            def
            state { inlets = inlets' }


updateOutlet
    :: forall d e
     . (Outlet d e -> Outlet d e)
    -> OutletPath
    -> Network d e
    -> RpdOp d e
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


disconnect :: forall d e. Outlet d e -> Inlet d e -> Patch d -> Patch d
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

instance eqInlet :: Eq (Inlet d e) where
    eq (Inlet pathA _ _) (Inlet pathB _ _) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d e) where
    eq (Outlet pathA _ _) (Outlet pathB _ _) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq (Link outletA inletA) (Link outletB inletB) = (outletA == outletB) && (inletA == inletB)
