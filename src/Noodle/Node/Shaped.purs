module Noodle.Node.Shaped
    where


import Noodle.Node as N
import Noodle.Channel as N

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))


type Inlet d = d /\ N.Channel d

type Outlet d = d /\ N.Channel d


type Node d =
    N.Node
        d
        (String /-> Inlet d) /\ (String /-> d)
