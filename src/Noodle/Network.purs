module Noodle.Network where


import Noodle.Node (Node, Link)
import Noodle.Patch (Patch)
import Noodle.Subpatch (Subpatch)

import Data.Map.Extra (type (/->))


data Network d a =
    Network
        (String /-> Patch d a)