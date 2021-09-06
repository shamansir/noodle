module Hydra.Queue where


import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested (type (/\))

import Hydra (Buffer, Texture)


type Queue =
    Buffer /-> Texture


empty :: Queue
empty = Map.empty


toUnfoldable :: forall f. Unfoldable f => Queue -> f (Buffer /\ Texture)
toUnfoldable = Map.toUnfoldable