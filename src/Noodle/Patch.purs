module Noodle.Patch where


import Noodle.Node

import Prelude ((#))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))


type InletPath = String /\ String
type OutletPath = String /\ String


data Patch d =
    Patch
        (String /-> Node d)
        ((OutletPath /\ InletPath) /-> Link)


empty :: forall d. Patch d
empty = Patch Map.empty Map.empty


addNode :: forall d. String -> Node d -> Patch d -> Patch d
addNode name node (Patch nodes links) =
    Patch
        (nodes # Map.insert name node)
        links


nodes :: forall d. Patch d -> Array (String /\ Node d)
nodes (Patch nodes links) = nodes # Map.toUnfoldable