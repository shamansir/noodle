module Noodle.Network where


import Prelude (($))

import Noodle.Node (Node, Link)
import Noodle.Patch (Patch)
import Noodle.Subpatch (Subpatch)

import Data.Map as Map
import Data.Map.Extra (type (/->))


data Network d a =
    Network
        (String /-> Patch d a)


empty :: forall d a. Network d a
empty = Network $ Map.empty