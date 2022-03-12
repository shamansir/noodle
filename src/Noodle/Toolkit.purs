module Noodle.Toolkit
  ( Toolkit(..)
  , nodeFamilies
  , spawn, spawn'
  )
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
data Toolkit state d = Toolkit d (Node.Family /-> NodeFn state d)



-- make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
-- make def = Toolkit def <<< Map.fromFoldable



spawn :: forall d. Node.Family -> Toolkit Unit d -> Effect (Maybe (Node Unit d))
spawn family = spawn' family unit


spawn' :: forall state d. Node.Family -> state -> Toolkit state d -> Effect (Maybe (Node state d))
spawn' family state (Toolkit def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (Node.make state def)
        # sequence


nodeFamilies :: forall state d. Toolkit state d -> Set Node.Family
nodeFamilies (Toolkit _ nodeDefs) =
    nodeDefs # Map.keys