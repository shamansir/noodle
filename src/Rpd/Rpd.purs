module Rpd
    ( Rpd, run, RpdEff, RpdEff'
    , Renderer, RenderEff
    , Flow, DataSource
    , Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , LazyPatch, LazyNode, LazyInlet, LazyOutlet
    , ProcessF
    , network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    , connect, connect'
    --, NetworkT, PatchT
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    , subscribeDataFlow
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array ((:), (!!), concatMap, mapWithIndex, catMaybes, modifyAt, foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\))
-- import Signal as S
-- import Signal.Channel as SC
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
-- import FRP.Event.Class (fold)
import Data.Foldable (fold)

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = Rpd (Network d)

type Flow d = Event d

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


-- type DataFlow d = Flow (DataMsg d)
type RpdEff e = (frp :: FRP | e)
type RpdEff' e = Eff (RpdEff e) Unit
type Canceller e =
    -- RpdEff e (RpdEff e Unit)
    Eff (RpdEff e) (RpdEff' e)
type RenderEff e =
    -- RpdEff e (RpdEff e Unit)
    Eff (RpdEff e) (RpdEff' e)


type Renderer d e = Network d -> RenderEff e


run :: forall d e. Renderer d e -> Network d -> RenderEff e
run renderer network = renderer network


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


subscribeDataFlow
    :: forall d e
     . Network d
    -> (d -> InletPath -> Eff (frp :: FRP | e) Unit)
    -> (d -> OutletPath -> Eff (frp :: FRP | e) Unit)
    -> Canceller e
subscribeDataFlow (Network { patches }) inletHandler outletHandler =
    let
        -- TODO: use lenses: https://github.com/purescript-contrib/purescript-lens
        --                   http://brianhamrick.com/blog/records-haskell-purescript
        allNodes = concatMap (\(Patch { nodes }) -> nodes) patches
        allInlets = concatMap (\(Node { inlets }) -> inlets) allNodes
        allOutlets = concatMap (\(Node { outlets }) -> outlets) allNodes
        adaptInletDataSources path dataSource =
            subscribe flow (\d -> inletHandler d path)
            where
                flow = case dataSource of
                    UserSource flow -> flow
                    OutletSource _ flow -> flow
        adaptOutletFlow path flow =
            subscribe flow (\d -> outletHandler d path)
        extractInletFlows = \(Inlet { path, sources }) ->
            map (adaptInletDataSources path) sources
        extractOutletFlows = \(Outlet { path, flow }) ->
            adaptOutletFlow path <$> flow
        inletFlows = concatMap extractInletFlows allInlets
        outletFlows = catMaybes $ map extractOutletFlows allOutlets
    in
        fold $ inletFlows <> outletFlows


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
    patch


connect' :: forall d. OutletPath -> InletPath -> Network d -> Maybe (Network d)
connect' outletPath inletPath network =
    findOutlet outletPath network
        >>= \(Outlet { flow }) -> flow
        >>= \signal ->
            let
                patchId = getPatchOfInlet inletPath
                newLink = Link outletPath inletPath
                network' = updatePatch
                    (\(Patch patch@{ links }) -> Patch patch { links = newLink : links })
                    patchId
                    network
                newSource = OutletSource outletPath signal
            in
                Just $ updateInlet
                    (\(Inlet inlet@{ sources }) ->
                        Inlet inlet { sources = newSource : sources})
                    inletPath
                    network'


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
