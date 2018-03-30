module Rpd
    ( Rpd, run
    , Renderer
    , RenderEff
    , Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , LazyPatch, LazyNode, LazyInlet, LazyOutlet
    , ProcessF
    , network, patch, node, inlet, inlet', outlet--, connect
    , subscribeAllData
    --, NetworkT, PatchT
    , PatchId, NodePath, InletPath, OutletPath
    ) where

import Control.Monad.Eff
import Data.Maybe
import Prelude

import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Array (concatMap, mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Signal as S
import Signal.Channel as SC

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = RpdT (Network d)


data PatchId = PatchId Int

data NodePath = NodePath PatchId Int

data InletPath = InletPath NodePath Int

data OutletPath = OutletPath NodePath Int


data Network d = Network (Array (Patch d)) -- (S.Signal d) -- change to info about where data flows
data Patch d = Patch PatchId String (Array (Node d)) (Array Link)
data Node d = Node NodePath String (Array (Inlet d)) (Array (Outlet d)) (ProcessF d) -- (S.Signal Unit) add node type just for tagging?
--data Node d = Node String (Map String d -> Map String d)
data Inlet d = Inlet InletPath String (S.Signal d) -- Channel?
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


type RenderEff e =
    Eff (channel :: SC.CHANNEL | e) (S.Signal (Eff ( channel :: SC.CHANNEL | e ) Unit))

type Renderer d e = Network d -> RenderEff e


run :: forall d e. Renderer d e -> Network d -> Eff (channel :: SC.CHANNEL | e) Unit
run renderer network = do
    signal <- renderer network
    S.runSignal signal


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
    \inletPath -> Inlet inletPath label dataSource


inlet' :: forall d. String -> d -> LazyInlet d
inlet' label defaultVal =
    inlet label $ S.constant defaultVal


outlet :: forall d. String -> LazyOutlet d
outlet label =
    \outletPath -> Outlet outletPath label Nothing


subscribeAllData
    :: forall d
     . Network d
    -- -> S.Signal (InletPath /\ d) /\ S.Signal (OutletPath /\ d)
    -> Maybe (S.Signal (InletPath /\ d))
subscribeAllData (Network patches) =
    let
        allNodes = concatMap (\(Patch patchId _ nodes _) -> nodes) patches
        allInlets = concatMap (\(Node _ _ inlets _ _) -> inlets) allNodes
        allOutlets = concatMap (\(Node _ _ _ outlets _) -> outlets) allNodes
        extractInletSignal = \(Inlet path _ signal) -> signal S.~> (\d -> path /\ d)
        --extractOutletSignal = \(Outlet path _ maybeSignal) -> maybeSignal S.~> (\d -> path /\ d)
    in
        S.mergeMany (map extractInletSignal allInlets)


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
