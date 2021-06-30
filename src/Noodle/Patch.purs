module Noodle.Patch where


import Noodle.Node

import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))


type InletPath = String /\ String
type OutletPath = String /\ String


data Patch d a =
    Patch (String /-> Node d a) ((OutletPath /\ InletPath) /-> Link)
