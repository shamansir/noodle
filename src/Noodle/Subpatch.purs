module Noodle.Subpatch where

import Data.Tuple.Nested (type (/\))


import Noodle.Patch (Patch)
import Noodle.Node (Node)


type Subpatch state m d = Node state m d /\ Patch state m d -- node represents inputs and outputs of the subpatch