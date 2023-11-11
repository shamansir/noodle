module Prev.Noodle.Subpatch where

import Data.Tuple.Nested (type (/\))


import Prev.Noodle.Patch (Patch)
import Prev.Noodle.Node (Node)


type Subpatch state d = Node state d /\ Patch state d -- node represents inputs and outputs of the subpatch