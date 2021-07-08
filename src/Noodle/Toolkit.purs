module Noodle.Toolkit
    where


import Effect (Effect)
import Prelude ((<<<), (#), map)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))
import Data.Maybe (Maybe)
import Data.Set (Set)

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Node.Define (Def)


data Toolkit d = Toolkit d (String /-> Def d)


make :: forall d. d -> Array (String /\ Def d) -> Toolkit d
make def = Toolkit def <<< Map.fromFoldable


spawn :: forall d. String -> Toolkit d -> Maybe (Effect (Node d))
spawn name (Toolkit def nodeDefs) =
    nodeDefs
        # Map.lookup name
        # map (Node.make def)


nodeNames :: forall d. Toolkit d -> Set String
nodeNames (Toolkit _ nodeDefs) =
    nodeDefs # Map.keys