module Noodle.Patch where


import Noodle.Node

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))


type InletPath = String /\ String
type OutletPath = String /\ String


data Patch d a =
    Patch
        (String /-> Node d a)
        ((OutletPath /\ InletPath) /-> Link)


empty :: forall d a. Patch d a
empty = Patch Map.empty Map.empty