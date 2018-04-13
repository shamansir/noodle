module Rpd
    ( Rpd, run
    , Renderer
    , RenderEff
    , Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , LazyPatch, LazyNode, LazyInlet, LazyOutlet
    , DataSignal, DataMsg(..), DataSource
    , ProcessF
    , network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet
    , connect, connect'
    --, NetworkT, PatchT
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    , subscribeDataSignal
    , ifFromInlet, ifFromOutlet
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array ((:), concatMap, mapWithIndex, catMaybes, modifyAt)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple.Nested (type (/\))
import Signal as S
import Signal.Channel as SC

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = Rpd (Network d)


-- TODO: FIXME store paths as arrays
data PatchId = PatchId Int
data NodePath = NodePath PatchId Int
data InletPath = InletPath NodePath Int
data OutletPath = OutletPath NodePath Int
data LinkId = LinkId Int


data DataSource d
    = UserSource (S.Signal d)
    | OutletSource OutletPath (S.Signal d)


-- TODO: normalize network, change to plain IDs maybe, or use paths as keys,
--       they implement Eq anyway

data Network d = Network
    { patches :: Array (Patch d)
    }
data Patch d = Patch
    { id :: PatchId
    , name :: String
    , nodes :: Array (Node d)
    , links :: Array Link
    }
data Node d = Node
    { path :: NodePath
    , name :: String
    , inlets :: Array (Inlet d)
    , outlets :: Array (Outlet d)
    , process :: ProcessF d -- (Map String d -> Map String d)
    }
-- S.constant is not able to send values afterwards, so we store the default value inside
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
    , signal :: Maybe (S.Signal d)
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

-- data UpdateSubj d = UNetwork | UPatch PatchId | UNode NodePath | ... | UBatch [UpdateSubj d]


-- type LazyNode d e = (PatchId -> WithId e (Node d))
type LazyPatch d = (PatchId -> Patch d) -- Reader monad
type LazyNode d = (NodePath -> Node d)
type LazyInlet d = (InletPath -> Inlet d)
type LazyOutlet d = (OutletPath -> Outlet d)


data DataMsg d
    = FromInlet InletPath d
    | FromOutlet OutletPath d


type DataSignal d = S.Signal (DataMsg d)


type RenderEff e =
    Eff (channel :: SC.CHANNEL | e) (S.Signal (Eff ( channel :: SC.CHANNEL | e ) Unit))


type Renderer d e = Network d -> RenderEff e


run :: forall d e. Renderer d e -> Network d -> Eff (channel :: SC.CHANNEL | e) Unit
run renderer network = do
    renderer network >>= S.runSignal


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


inlet' :: forall d. String -> S.Signal d -> LazyInlet d
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


inletWithDefault' :: forall d. String -> d -> S.Signal d -> LazyInlet d
inletWithDefault' label defaultVal dataSource  =
    inlet_ label (Just defaultVal) [ UserSource dataSource ]


outlet :: forall d. String -> LazyOutlet d
outlet label =
    \outletPath ->
        Outlet
            { path : outletPath
            , label
            , signal : Nothing
            }


subscribeDataSignal
    :: forall d
     . Network d
    -- -> (d -> InletPath -> Eff e Unit)
    -- -> (d -> OutletPath -> Eff e Unit)
    -> Maybe (DataSignal d)
subscribeDataSignal (Network { patches }) =
    let
        allNodes = concatMap (\(Patch { nodes }) -> nodes) patches
        allInlets = concatMap (\(Node { inlets }) -> inlets) allNodes
        allOutlets = concatMap (\(Node { outlets }) -> outlets) allNodes
        adaptInletDataSources path dataSource =
            signal S.~> (\d -> FromInlet path d)
            where
                signal =
                    case dataSource of
                        UserSource signal -> signal
                        OutletSource _ signal -> signal
        adaptOutletSignal path signal =
            signal S.~> (\d -> FromOutlet path d)
        extractInletSignals = \(Inlet { path, sources }) ->
            S.mergeMany $ map (adaptInletDataSources path) sources
        extractOutletSignals = \(Outlet { path, signal }) ->
            adaptOutletSignal path <$> signal
        inletSignals = catMaybes $ map extractInletSignals allInlets
        outletSignals = catMaybes $ map extractOutletSignals allOutlets
    in
        S.mergeMany $ inletSignals <> outletSignals


-- returns data extracted from data message if it came from the specified inlet
-- or else returns `Nothing``
ifFromInlet :: forall d. InletPath -> DataMsg d -> Maybe d
ifFromInlet path (FromInlet inletPath d) | inletPath == path = Just d
ifFromInlet _ _ = Nothing


-- returns data extracted from data message if it came from the specified outlet
-- or else returns `Nothing``
ifFromOutlet :: forall d. OutletPath -> DataMsg d -> Maybe d
ifFromOutlet path (FromOutlet outletPath d) | outletPath == path = Just d
ifFromOutlet _ _ = Nothing


isNodeInPatch :: NodePath -> PatchId -> Boolean
isNodeInPatch (NodePath patchId' _) patchId = patchId == patchId'


isInletInPatch :: InletPath -> PatchId -> Boolean
isInletInPatch (InletPath nodePath _) patchId = isNodeInPatch nodePath patchId


isOutletInPatch :: OutletPath -> PatchId -> Boolean
isOutletInPatch (OutletPath nodePath _) patchId = isNodeInPatch nodePath patchId


isInletInNode :: InletPath -> NodePath -> Boolean
isInletInNode (InletPath nodePath' _) nodePath = nodePath == nodePath'


isOutletInNode :: OutletPath -> NodePath -> Boolean
isOutletInNode (OutletPath nodePath' _) nodePath = nodePath == nodePath'


notInTheSameNode :: InletPath -> OutletPath -> Boolean
notInTheSameNode (InletPath iNodePath _) (OutletPath oNodePath _) = iNodePath /= oNodePath


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
    patch


connect' :: forall d. OutletPath -> InletPath -> Network d -> Network d
connect' outletPath inletPath network =
    network
    -- let
    --     patchId = getPatchOfInlet inletPath
    --     newLink = Link outletPath inletPath
    --     network' = updatePatch
    --         (\(Patch id title nodes links) -> Patch id title nodes $ newLink : links)
    --         patchId
    --         network
    --     newSource = UserSource $ S.constant "test" -- FIXME: use outletStream
    -- in
    --     updateInlet
    --         (\(Inlet path label defaultVal dataSources) ->
    --             Inlet path label defaultVal $ newSource : dataSources)
    --         inletPath
    --         network'


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


-- | Get the current value of a signal. Should be in purescript-signal, pending
-- https://github.com/bodil/purescript-signal/pull/60
foreign import get :: forall e a. S.Signal a -> Eff e a


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



instance showDataMsg :: Show d => Show (DataMsg d) where
    show (FromInlet inletPath d) = show inletPath <> " " <> show d
    show (FromOutlet outletPath d) = show outletPath <> " " <> show d


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
