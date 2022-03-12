module Noodle.Subpatch where

import Data.Tuple.Nested (type (/\))


import Noodle.Patch (Patch)
import Noodle.Node (Node)


type Subpatch state d = Node state d /\ Patch state d -- node represents inputs and outputs of the subpatch