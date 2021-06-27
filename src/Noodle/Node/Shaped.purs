module Noodle.Node.Shaped
    where

import Prelude (pure, ($))


import Noodle.Node as N
import Noodle.Node (Receive, Pass)
import Noodle.Channel as N
import Noodle.Shape as N

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)


{-
class IsShaped m where
    inlet :: forall d. String -> m -> Maybe (Inlet d)
    outlet :: forall d. String -> m -> Maybe (Outlet d)
    inlets :: forall d. m -> Array (String /\ Inlet d)
    outlets :: forall d. m -> Array (String /\ Outlet d)

ShapeOf.inlet
ShapeOf.oulet
ShapeOf.inlets
ShapeOf.oulets
etc.
-}


type Node d =
    N.Node
        d
        ((String /-> N.Shape d) /\ (String /-> N.Shape d))


make
    :: forall d
    .  d
    -> Array (String /\ N.Shape d)
    -> Array (String /\ N.Shape d)
    -> (Receive d -> Pass d)
    -> Effect (Node d)
make def inlets outlets fn = N.empty (Map.empty /\ Map.empty) def


makeEff
    :: forall d
    .  d
    -> Array (String /\ N.Shape d)
    -> Array (String /\ N.Shape d)
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d)
makeEff def inlets outlets fn = N.empty (Map.empty /\ Map.empty) def