module Noodle.Toolkit
    where


import Effect (Effect)
import Prelude ((<<<), (#), map)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))
import Data.Maybe (Maybe)

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Node.Define (Def)


data Toolkit d a = Toolkit (String /-> Def a d)


data Renderer d a view = Renderer (String -> Node d a -> view)


make :: forall d a. Array (String /\ Def a d) -> Toolkit d a
make = Toolkit <<< Map.fromFoldable


spawn :: forall d a. String -> d -> Toolkit d a -> Maybe (Effect (Node d a))
spawn name def (Toolkit nodeDefs) =
    nodeDefs
        # Map.lookup name
        # map (Node.make def)