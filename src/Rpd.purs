module Rpd
    ( Rpd, Network, Patch, Node, Inlet, Outlet, Link
    , network, patch, node, inlet, inlet', outlet, connect
    ) where


import Control.Monad.Eff
import Data.Maybe

import Data.Tuple.Nested (type (/\))
import Prelude (Unit, id, ($), bind, pure)
import Signal as S
import Signal.Channel as SC

type ProcessF d = (Array (String /\ d) -> Array (String /\ d))

type AdaptF d = (d -> d)

data Rpd d = RpdT (Network d)

data Network d = Network (Array (Patch d)) -- (S.Signal d) -- change to info about where data flows
data Patch d = Patch String (Array (Node d)) (Array Link)
data Node d = Node String (Array (Inlet d)) (Array (Outlet d)) (ProcessF d) -- (S.Signal Unit) add node type just for tagging?
--data Node d = Node String (Map String d -> Map String d)
data Inlet d = Inlet String (S.Signal d)
--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d = Outlet String (Maybe (S.Signal d))
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


inlet :: forall d. String -> S.Signal d -> Inlet d
inlet label dataSource =
    Inlet label dataSource


inlet' :: forall d. String -> d -> Inlet d
inlet' label defaultVal =
    Inlet label $ S.constant defaultVal


outlet :: forall d. String -> Outlet d
outlet label =
    Outlet label Nothing


-- connect inside a Patch??
connect :: forall d e. Inlet d -> Outlet d -> d -> Eff ( channel :: SC.CHANNEL | e ) (SC.Channel d)
connect inlet outlet defaultVal = do
    channel <- SC.channel defaultVal
    pure channel

