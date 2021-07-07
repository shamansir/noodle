module Noodle.Subpatch where

import Data.Tuple.Nested (type (/\))


import Noodle.Patch (Patch)
import Noodle.Node (Node)


type Subpatch d = Node d /\ Patch d -- node represents inputs and outputs of the subpatch