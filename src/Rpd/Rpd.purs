module Rpd
    ( Rpd, run, RpdEff, RpdEffE
    , Renderer, RenderEff
    , DataSource
    , Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , LazyPatch, LazyNode, LazyInlet, LazyOutlet
    , ProcessF
    , network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    , connect, connect', disconnect, disconnect', disconnectTop, processWith
    --, NetworkT, PatchT
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    -- , subscribeDataFlow,
    , subscribeAll, subscribeTop
    , Canceler, Subscriber, Cancelers, initCancelers
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    -- , findTopSource, getFlowOf
    ) where

import Prelude

import Data.Monoid (mempty)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (isJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((:), (!!), concatMap, mapWithIndex, catMaybes, mapMaybe, modifyAt, foldr, findMap, delete, filter, head)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Tuple.Nested ((/\), type (/\))
-- import Signal as S
-- import Signal.Channel as SC
import Rpd.Flow (FLOW, Flow, FlowEffE, subscribe)
-- import FRP.Event.Class (fold)
import Data.Foldable (fold)

-- type ProcessF d = (Array (String /\ d) -> Array (String /\ d))
type ProcessF d = (Map String d -> Map String d)
type ProcessF' d = (String -> d -> d)
type ProcessF'' d = (String -> d -> Maybe d)

type AdaptF d = (d -> d)

data Rpd d = Rpd (Network d)

data PatchId = PatchId Int
data NodePath = NodePath PatchId Int
data InletPath = InletPath NodePath Int
data OutletPath = OutletPath NodePath Int
data LinkId = LinkId Int


data DataSource d
    = UserSource (Flow d)
    | OutletSource OutletPath (Flow d)


-- TODO: normalize network, change to plain IDs maybe, or use paths as keys,
--       they implement Eq anyway

data Network d = Network
    { patches :: Array (Patch d)
    }
data Patch d = Patch
    { id :: PatchId
    , name :: String
    , nodes :: Array (Node d)
    , links :: Array Link -- TODO: links partly duplicate Inlet: sources, aren't they?
    -- TODO: maybe store Connections: Map InletPath (Array DataSource)
    }
data Node d = Node
    { path :: NodePath
    , name :: String
    , inlets :: Array (Inlet d)
    , outlets :: Array (Outlet d)
    , process :: ProcessF d -- (Map String d -> Map String d)
    }
-- S.constant is not able to send values afterwards, so we store the default value inside
-- TODO: inlet sources should be a set of outletPaths, so outlet-inlet pairs would be unique
data Inlet d = Inlet
    { path :: InletPath
    , label :: String
    , default :: Maybe d
    , sources :: Array (DataSource d)
    -- Maybe (AdaptF d)
    }
--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d = Outlet
    { path :: OutletPath
    , label :: String
    , flow :: Maybe (Flow d)
    }
data Link = Link OutletPath InletPath


-- data NormalizedNetwork d =
--     NormalizedNetwork
--         (Array (Patch' d))
--         (Array (Node' d))
--         (Array (Inlet' d))
--         (Array (Outlet' d))
--         (Array (Link' d))

-- type WithId e a = Eff ( random :: RANDOM | e ) a


 -- Reader monad
type LazyPatch d = (PatchId -> Patch d)
type LazyNode d = (NodePath -> Node d)
type LazyInlet d = (InletPath -> Inlet d)
type LazyOutlet d = (OutletPath -> Outlet d)


-- data DataMsg d
--     = FromInlet InletPath d
--     | FromOutlet OutletPath d


-- FIXME: define our own RPD effect

-- type DataFlow d = Flow (DataMsg d)
type RpdEffE e = FlowEffE (console :: CONSOLE | e)
type RpdEff e v = Eff (RpdEffE e) v
type Canceler e =
    RpdEff e Unit
    -- (Unit -> RpdEff e (RpdEff e Unit))
type Subscriber e =
    RpdEff e (Canceler e)
type RenderEff e =
    RpdEff e Unit
type Cancelers e =
    Map OutletPath (Canceler e) /\ Map InletPath (Array (Canceler e))


type Renderer d e = Network d -> RenderEff e


run :: forall d e. Renderer d e -> Network d -> RenderEff e
run renderer nw = renderer nw


network :: forall d. Array (LazyPatch d) -> Network d
network lazyPatches =
    Network { patches }
    where
        patches = mapWithIndex
            (\idx lazyPatch -> lazyPatch $ PatchId idx) lazyPatches


patch :: forall d e. String -> Array (LazyNode d) -> LazyPatch d
patch name lazyNodes =
    \patchId ->
        let
            nodes = mapWithIndex
                (\idx lazyNode ->
                    lazyNode (NodePath patchId idx)) lazyNodes
        in
            Patch
                { id : patchId
                , name
                , nodes
                , links : []
                }


node :: forall d. String -> Array (LazyInlet d) -> Array (LazyOutlet d) -> LazyNode d
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
                , process : id
                }


inlet :: forall d. String -> LazyInlet d
inlet label =
    inlet_ label Nothing []


inlet' :: forall d. String -> Flow d -> LazyInlet d
inlet' label dataSource =
    inlet_ label Nothing [ UserSource dataSource ]


-- should not be exposed
inlet_ :: forall d. String -> Maybe d -> Array (DataSource d) -> LazyInlet d
inlet_ label maybeDefault sources =
    \inletPath ->
        Inlet
            { path : inletPath
            , label
            , default : maybeDefault
            , sources
            }


inletWithDefault :: forall d. String -> d -> LazyInlet d
inletWithDefault label defaultVal =
    -- inlet_ label defaultVal $ S.constant defaultVal
    inlet_ label (Just defaultVal) [ ]


inletWithDefault' :: forall d. String -> d -> Flow d -> LazyInlet d
inletWithDefault' label defaultVal dataSource  =
    inlet_ label (Just defaultVal) [ UserSource dataSource ]


outlet :: forall d. String -> LazyOutlet d
outlet label =
    \outletPath ->
        Outlet
            { path : outletPath
            , label
            , flow : Nothing
            }


-- TODO: remove, outlets should only produce values from `process` function
outlet' :: forall d. String -> Flow d -> LazyOutlet d
outlet' label flow =
    \outletPath ->
        Outlet
            { path : outletPath
            , label
            , flow : Just flow
            }


-- subscribeDataFlow
--     :: forall d e
--      . (d -> InletPath -> RpdEff e Unit)
--     -> (d -> OutletPath -> RpdEff e Unit)
--     -> Network d
--     -> Subscriber e
-- subscribeDataFlow inletHandler outletHandler (Network { patches }) = do
--     pure $ do
--         log "!!! subscribing data flow"
--         fold $ inletFlows <> outletFlows
--     where
--         -- TODO: use lenses: https://github.com/purescript-contrib/purescript-lens
--         --                   http://brianhamrick.com/blog/records-haskell-purescript
--         allNodes = concatMap (\(Patch { nodes }) -> nodes) patches
--         allInlets = concatMap (\(Node { inlets }) -> inlets) allNodes
--         allOutlets = concatMap (\(Node { outlets }) -> outlets) allNodes
--         adaptInletDataSources path dataSource =
--             subscribe flow (\d -> inletHandler d path)
--             where
--                 flow = case dataSource of
--                     UserSource flow -> flow
--                     OutletSource _ flow -> flow
--         adaptOutletFlow path flow =
--             subscribe flow (\d -> outletHandler d path)
--         extractInletFlows = \(Inlet { path, sources }) ->
--             map (adaptInletDataSources path) sources
--         extractOutletFlows = \(Outlet { path, flow }) ->
--             adaptOutletFlow path <$> flow
--         inletFlows = concatMap extractInletFlows allInlets
--         outletFlows = catMaybes $ map extractOutletFlows allOutlets


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


getFlowOf :: forall d. DataSource d -> Flow d
getFlowOf dataSource =
    case dataSource of
        UserSource flow -> flow
        OutletSource _ flow -> flow


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


-- TODO: change return type of the functions below to Maybe,
-- to identify the case when subject wasn't modified

updatePatch :: forall d. (Patch d -> Patch d) -> PatchId -> Network d -> Network d
updatePatch updater (PatchId patchId) (Network network) =
    Network network
        { patches = fromMaybe network.patches
            $ modifyAt patchId updater network.patches
        }


updateNode :: forall d. (Node d -> Node d) -> NodePath -> Network d -> Network d
updateNode updater (NodePath patchId nodeId) network =
    updatePatch (\(Patch patch) ->
        Patch patch
            { nodes = fromMaybe patch.nodes
                $ modifyAt nodeId updater patch.nodes
            }
    ) patchId network


updateInlet :: forall d. (Inlet d -> Inlet d) -> InletPath -> Network d -> Network d
updateInlet updater (InletPath nodePath inletId) network =
    updateNode (\(Node node) ->
        Node node
            { inlets = fromMaybe node.inlets
                $ modifyAt inletId updater node.inlets
            }
    ) nodePath network


updateOutlet :: forall d. (Outlet d -> Outlet d) -> OutletPath -> Network d -> Network d
updateOutlet updater (OutletPath nodePath outletId) network =
    updateNode (\(Node node) ->
        Node node
            { outlets = fromMaybe node.outlets
                $ modifyAt outletId updater node.outlets
            }
    ) nodePath network


connect :: forall d. Outlet d -> Inlet d -> Patch d -> Patch d
connect outlet inlet patch =
    patch -- FIXME: implement


processWith :: forall d. ProcessF d -> LazyNode d -> LazyNode d
processWith processF nodeF =
    \path -> case nodeF path of Node node -> Node node { process = processF }


processWith' :: forall d. ProcessF' d -> LazyNode d -> LazyNode d
processWith' processF =
    processWith f
    where
        f inputs = Map.mapWithKey processF inputs


processWith'' :: forall d. ProcessF'' d -> d -> LazyNode d -> LazyNode d
processWith'' processF default =
    processWith f
    where
        f inputs = map (fromMaybe default)
                    $ Map.filter isJust
                    $ Map.mapWithKey processF inputs


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


disconnect :: forall d. Outlet d -> Inlet d -> Patch d -> Patch d
disconnect outlet inlet patch =
    patch -- FIXME: implement


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


disconnectTop :: forall d. InletPath -> Network d -> Maybe (Network d)
disconnectTop inletPath network =
    -- TODO: optimize with searching last and updating simultaneously
    findTopConnection inletPath network
        >>= (\outletPath -> disconnect' outletPath inletPath network)


-- disconnectLast' :: forall d. InletPath -> Network d -> Maybe (Network d)

-- getConnections :: Node -> (Map OutletPath InletPath) or (Array Link)

-- getConnections :: Patch -> (Map OutletPath InletPath) or (Array Link)

-- subscribeAll :: Network -> f -> Map (Inlet (Canceler e)) /\ Map (Outlet (Canceler e))

-- subscribeNode :: Node -> f -> Map (Inlet (Canceler e)) /\ Map (Outlet (Canceler e))

-- subscribePatch :: Patch -> f -> Map (Inlet (Canceler e)) /\ Map (Outlet (Canceler e))

-- subscribeInlets :: Node -> f -> Map (Inlet (Canceler e))


initCancelers :: forall e. Cancelers e
initCancelers = Map.empty /\ Map.empty


subscribeAll
    :: forall d e
     . (InletPath -> DataSource d -> d -> RpdEff e Unit)
    -> (OutletPath -> d -> RpdEff e Unit)
    -> Network d
    -> Cancelers e
subscribeAll inletHandler outletHandler (Network { patches }) =
    let
        allNodes = concatMap (\(Patch { nodes }) -> nodes) patches
        outletF = \o@(Outlet { path }) ->
                (/\) path <$> subscribeOutlet' (outletHandler path) o
        inletF = \i@(Inlet { path }) ->
                path /\ subscribeInlet' (inletHandler path) i
        -- allInletsAndOutlets = concatMap
        --     (\(Node { inlets, outlets }) -> inlets /\ outlets) allNodes
        allInlets = concatMap (\(Node { inlets }) -> inlets) allNodes
        allOutlets = concatMap (\(Node { outlets }) -> outlets) allNodes
    in (fromFoldable $ mapMaybe outletF allOutlets)
    /\ (fromFoldable $ map inletF allInlets)


subscribeNode
    :: forall d e
     . (InletPath -> DataSource d -> d -> RpdEff e Unit)
    -> (OutletPath -> d -> RpdEff e Unit)
    -> NodePath
    -> Network d
    -> Maybe (Cancelers e)
subscribeNode inletHandler outletHandler nodePath network =
    subscribeNode' inletHandler outletHandler <$> findNode nodePath network


subscribeNode'
    :: forall d e
     . (InletPath -> DataSource d -> d -> RpdEff e Unit)
    -> (OutletPath -> d -> RpdEff e Unit)
    -> Node d
    -> Cancelers e
subscribeNode' inletHandler outletHandler (Node { outlets, inlets }) =
    let
        outletF = \o@(Outlet { path }) ->
                (/\) path <$> subscribeOutlet' (outletHandler path) o
        inletF = \i@(Inlet { path }) ->
                path /\ subscribeInlet' (inletHandler path) i
    in (fromFoldable $ mapMaybe outletF outlets)
    /\ (fromFoldable $ map inletF inlets)


subscribeOutlet
    :: forall d e
     . (d -> RpdEff e Unit)
    -> OutletPath
    -> Network d
    -> Maybe (Canceler e)
subscribeOutlet f outletPath network =
    findOutlet outletPath network >>= subscribeOutlet' f


subscribeOutlet'
    :: forall d e
     . (d -> RpdEff e Unit)
    -> Outlet d
    -> Maybe (Canceler e)
subscribeOutlet' f (Outlet { path, flow : maybeFlow }) =
    case maybeFlow of
        Just flow -> do
            -- log $ "subscribeOutlet " <> show path
            let
                cancel = do
                    subEff <- subscribe flow f
                    cancel <- subEff
                    pure cancel
            pure cancel
        Nothing -> pure $ pure unit
    -- flow <- maybeFlow
    -- pure $ do
    --     log $ "subscribeOutlet " <> show path
    --     cancel <- subscribe flow f
    --     pure cancel


subscribeInlet
    :: forall d e
     . (DataSource d -> d -> RpdEff e Unit)
    -> InletPath
    -> Network d
    -> Maybe (Array (Canceler e))
subscribeInlet f inletPath network =
    subscribeInlet' f <$> findInlet inletPath network


subscribeInlet'
    :: forall d e
     . (DataSource d -> d -> RpdEff e Unit)
    -> Inlet d
    -> Array (Canceler e)
subscribeInlet' f (Inlet { path, sources }) =
    map
        (\source -> do
            log $ "subscribeInlet " <> show path
            subEff <- subscribe (getFlowOf source) (f source)
            cancel <- subEff
            pure cancel
        ) sources


subscribeTop
    :: forall d e
     . (DataSource d -> d -> RpdEff e Unit)
    -> InletPath
    -> Network d
    -> Maybe (Canceler e)
subscribeTop f inletPath network =
    findInlet inletPath network >>= subscribeTop' f


subscribeTop'
    :: forall d e
     . (DataSource d -> d -> RpdEff e Unit)
    -> Inlet d
    -> Maybe (Canceler e)
subscribeTop' f (Inlet { sources }) =
    (\topSource -> do
        subEff <- subscribe (getFlowOf topSource) (f topSource)
        cancel <- subEff
        pure cancel
    ) <$> head sources



-- subscribeOutlet :: Outlet -> f -> Canceler e


-- TODO: findLink :: forall d. InletPath -> Network d -> Maybe (Link d)


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
    eq (PatchId a) (PatchId b) = (a == b)

instance eqNodePath :: Eq NodePath where
    eq (NodePath pa a) (NodePath pb b) = (pa == pb) && (a == b)

instance eqInletPath :: Eq InletPath where
    eq (InletPath na a) (InletPath nb b) = (na == nb) && (a == b)

instance eqOutletPath :: Eq OutletPath where
    eq (OutletPath na a) (OutletPath nb b) = (na == nb) && (a == b)


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
        compare (unpackOutletPath outletPath1)  (unpackOutletPath outletPath2)


-- TODO: create HasId / HasPath typeclass
instance eqPatch :: Eq (Patch d) where
    eq (Patch { id : idA }) (Patch { id : idB }) = (idA == idB)

instance eqNode :: Eq (Node d) where
    eq (Node { path : pathA }) (Node { path : pathB }) = (pathA == pathB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet { path : pathA }) (Inlet { path : pathB }) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet { path : pathA }) (Outlet { path : pathB }) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq (Link outletA inletA) (Link outletB inletB) = (outletA == outletB) && (inletA == inletB)
