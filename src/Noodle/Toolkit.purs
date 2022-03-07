module Noodle.Toolkit
    where


import Prelude ((<<<), (#), map, ($))

import Effect (Effect)

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Traversable (sequence)

import Noodle.Node (Node)
import Noodle.Node as Node


data Toolkit d = Toolkit d (Node.Id /-> Def d)


make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
make def = Toolkit def <<< Map.fromFoldable


spawn :: forall d. Node.Family -> Toolkit d -> Effect (Maybe (Node d))
spawn family (Toolkit def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (Node.make def)
        # map (map $ Node.markFamily family)
        # sequence


nodeFamilies :: forall d. Toolkit d -> Set Node.Family
nodeFamilies (Toolkit _ nodeDefs) =
    nodeDefs # Map.keys