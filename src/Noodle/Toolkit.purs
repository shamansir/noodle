module Noodle.Toolkit
    where


import Prelude ((<<<), (#), map, ($), unit, Unit)

import Effect (Effect)
import Effect.Aff (Aff)

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Traversable (sequence)

import Noodle.Node (Node, NodeFn)
import Noodle.Node as Node


{- TODO: maybe `m` would be just `Effect` or `Aff` here -}
data Toolkit state m d = Toolkit d (Node.Family /-> NodeFn state m d)


-- make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
-- make def = Toolkit def <<< Map.fromFoldable


spawn :: forall state d. Node.Family -> state -> Toolkit state Aff d -> Effect (Maybe (Node state Aff d))
spawn family state (Toolkit def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (Node.make state def)
        # sequence


spawn' :: forall d. Node.Family -> Toolkit Unit Aff d -> Effect (Maybe (Node Unit Aff d))
spawn' family = spawn family unit


nodeFamilies :: forall state m d. Toolkit state m d -> Set Node.Family
nodeFamilies (Toolkit _ nodeDefs) =
    nodeDefs # Map.keys