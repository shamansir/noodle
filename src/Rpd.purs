module Rpd
    ( Rpd, Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)
    , ProcessF
    , network, patch, node, inlet, inlet', outlet--, connect
    --, NetworkT, PatchT
    , PatchId, NodePath, InletPath, OutletPath
    ) where


import Control.Monad.Eff
import Data.Maybe

import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Either.Nested (either10)
import Data.Tuple.Nested (type (/\))
import Prelude (Unit, id, ($), bind, pure)
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


type WithId e a = Eff ( random :: RANDOM | e ) a


addId :: forall a e. (Int -> a) -> WithId e a
addId f = do
    id <- randomInt 0 100
    pure $ f id


network :: forall d. Array (Patch d) -> Network d
network patches =
    Network patches


patch :: forall d e. String -> Array (Node d) -> WithId e (Patch d)
patch name nodes =
    addId $ \patchId ->
        Patch (PatchId patchId) name nodes []


node :: forall d e. String -> Array (Inlet d) -> Array (Outlet d) -> (PatchId -> WithId e (Node d))
node name inlets outlets =
    \patchId -> addId $
        \nodeId -> Node (NodePath patchId nodeId) name inlets outlets id


inlet :: forall d e. String -> S.Signal d -> (NodePath -> WithId e (Inlet d))
inlet label dataSource =
    \nodePath -> addId $
        \inletId -> Inlet (InletPath nodePath inletId) label dataSource


inlet' :: forall d e. String -> d -> (NodePath -> WithId e (Inlet d))
inlet' label defaultVal =
    inlet label $ S.constant defaultVal


outlet :: forall d e. String -> (NodePath -> WithId e (Outlet d))
outlet label =
    \nodePath -> addId $ \outletId -> Outlet (OutletPath nodePath outletId) label Nothing


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

