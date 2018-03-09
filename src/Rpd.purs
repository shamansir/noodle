module Rpd
    ( Rpd, Network, Patch, Node, Inlet, Outlet, Link
    , network, patch, node, inlet, outlet --, link
    ) where

import Prelude (id, ($))

--import Data.Tuple
import Data.Tuple.Nested (type (/\))

import Signal as S

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = RpdT (Network d)

data Network d = Network (Array (Patch d)) -- (S.Signal d) -- change to info about where data flows
data Patch d = Patch String (Array (Node d)) (Array Link)
data Node d = Node String (Array (Inlet d)) (Array (Outlet d)) (ProcessF d) -- (S.Signal Unit) add node type just for tagging?
--data Node d = Node String (Map String d -> Map String d)
data Inlet d = Inlet String (S.Signal d)
--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d = Outlet String -- (S.Signal d)
data Link = LinkT

network :: forall d. Array (Patch d) -> Network d
network patches =
    Network patches


patch :: forall d. String -> Array (Node d) -> Patch d
patch name nodes =
    Patch name nodes []


node :: forall d. String -> Array (Inlet d) -> Array (Outlet d) -> Node d
node name inlets outlets =
    Node name inlets outlets id


inlet :: forall d. String -> d -> Inlet d
inlet label defaultVal =
    Inlet label $ S.constant defaultVal


outlet :: forall d. String -> Outlet d
outlet label =
    Outlet label
