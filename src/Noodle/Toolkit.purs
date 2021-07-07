module Noodle.Toolkit
    where

import Prelude ((<<<))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))

import Noodle.Node (Node)


data Toolkit d a = Toolkit (String /-> Node d a)


data Renderer d a view = Renderer (String -> Node d a -> view)



make :: forall d a. Array (String /\ Node d a) -> Toolkit d a
make = Toolkit <<< Map.fromFoldable