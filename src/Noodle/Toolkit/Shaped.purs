module Noodle.Toolkit.Shaped where


import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))

import Noodle.Toolkit as T
import Noodle.Node.Shaped as D
import Noodle.Node.Shape as Node


type Toolkit d = T.Toolkit d (Node.Shape d)


type Renderer d view = T.Renderer d (Node.Shape d) view


make :: forall d. Array (String /\ D.Def d) -> Toolkit d
make = T.make


spawn :: forall d. String -> d -> Toolkit d -> Maybe (Effect (D.Node d))
spawn = T.spawn