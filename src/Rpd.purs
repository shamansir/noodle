module Rpd
    ( Rpd, run
    , Renderer
    , RenderEff
    , Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , LazyPatch, LazyNode, LazyInlet, LazyOutlet
    , DataSignal, DataMsg(..)
    , ProcessF
    , network, patch, node, inlet, inlet', outlet--, connect
    --, NetworkT, PatchT
    , PatchId, NodePath, InletPath, OutletPath, LinkId
    , patchId, nodePath, inletPath, outletPath
    , ifFromInlet, ifFromOutlet
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Array (concatMap, mapWithIndex)
import Data.Tuple.Nested (type (/\))
import Signal as S
import Signal.Channel as SC

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = RpdT (Network d)


data PatchId = PatchId Int
data NodePath = NodePath PatchId Int
data InletPath = InletPath NodePath Int
data OutletPath = OutletPath NodePath Int
data LinkId = LinkId Int


data DataSource d
    = UserSource (S.Signal d)
    | OutletSource OutletPath (S.Signal d)


data Network d = Network (Array (Patch d)) -- (S.Signal d) -- change to info about where data flows
data Patch d = Patch PatchId String (Array (Node d)) (Array Link)
data Node d = Node NodePath String (Array (Inlet d)) (Array (Outlet d)) (ProcessF d) -- (S.Signal Unit) add node type just for tagging?
--data Node d = Node String (Map String d -> Map String d)
-- S.constant is not able to send values afterwards, so we store the default value inside
data Inlet d = Inlet InletPath String (Maybe d) (S.Signal d)
--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d = Outlet OutletPath String (Maybe (S.Signal d))
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


type Renderer d e = Maybe (DataSignal d) -> Network d -> RenderEff e


run :: forall d e. Renderer d e -> Network d -> Eff (channel :: SC.CHANNEL | e) Unit
run renderer network = do
    let maybeDataSignal = prepareDataSignal network
    renderSignal <- renderer maybeDataSignal network
    S.runSignal renderSignal


network :: forall d. Array (LazyPatch d) -> Network d
network lazyPatches =
    Network patches
    where
        patches = mapWithIndex (\idx lazyPatch -> lazyPatch $ PatchId idx) lazyPatches


patch :: forall d e. String -> Array (LazyNode d) -> LazyPatch d
patch name lazyNodes =
    \patchId ->
        let
            nodes = mapWithIndex (\idx lazyNode -> lazyNode (NodePath patchId idx)) lazyNodes
        in
            Patch patchId name nodes []


node :: forall d. String -> Array (LazyInlet d) -> Array (LazyOutlet d) -> LazyNode d
node name lazyInlets lazyOutlets =
    \nodePath ->
        let
            inlets = mapWithIndex (\idx lazyInlet -> lazyInlet (InletPath nodePath idx)) lazyInlets
            outlets = mapWithIndex (\idx lazyOutlet -> lazyOutlet (OutletPath nodePath idx)) lazyOutlets
        in
            Node nodePath name inlets outlets id


inlet :: forall d. String -> S.Signal d -> LazyInlet d
inlet label dataSource =
    \inletPath -> Inlet inletPath label Nothing dataSource


inlet' :: forall d. String -> d -> LazyInlet d
inlet' label defaultVal =
    inlet_ label defaultVal $ S.constant defaultVal


inlet_ :: forall d. String -> d -> S.Signal d -> LazyInlet d
inlet_ label defaultVal dataSource =
    \inletPath -> Inlet inletPath label (Just defaultVal) dataSource


outlet :: forall d. String -> LazyOutlet d
outlet label =
    \outletPath -> Outlet outletPath label Nothing


prepareDataSignal
    :: forall d
     . Network d
    -- -> S.Signal (InletPath /\ d) /\ S.Signal (OutletPath /\ d)
    -> Maybe (DataSignal d)
prepareDataSignal (Network patches) =
    let
        allNodes = concatMap (\(Patch patchId _ nodes _) -> nodes) patches
        allInlets = concatMap (\(Node _ _ inlets _ _) -> inlets) allNodes
        allOutlets = concatMap (\(Node _ _ _ outlets _) -> outlets) allNodes
        extractInletSignal = \(Inlet path _ _ signal) -> signal S.~> (\d -> FromInlet path d)
        --extractOutletSignal = \(Outlet path _ maybeSignal) -> maybeSignal S.~> (\d -> path /\ d)
    in
        S.mergeMany (map extractInletSignal allInlets)


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


-- updatePatch

-- updateNode

-- updateInlet

-- updateOutlet


connect :: forall d. Outlet d -> Inlet d -> Patch d -> Patch d
connect outlet inlet patch =
    patch


connect' :: forall d. OutletPath -> InletPath -> Network d -> Network d
connect' outletPath inletPath network =
    network


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
    compare (NodePath pa a) (NodePath pb b) =
        compare (compare pa pb) (compare a b)

instance ordInletPath :: Ord InletPath where
    compare (InletPath na a) (InletPath nb b) =
        compare (compare na nb) (compare a b)

instance ordOutletPath :: Ord OutletPath where
    compare (OutletPath na a) (OutletPath nb b) =
        compare (compare na nb) (compare a b)
