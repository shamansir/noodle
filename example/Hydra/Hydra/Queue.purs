module Hydra.Queue where


import Prelude (flip)

import Data.Maybe (Maybe)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested (type (/\))

import Hydra (Buffer(..), Texture)


type Queue =
    Buffer /-> Texture


empty :: Queue
empty = Map.empty


isEmpty :: Queue -> Boolean
isEmpty = Map.isEmpty


toUnfoldable :: forall f. Unfoldable f => Queue -> f (Buffer /\ Texture)
toUnfoldable = Map.toUnfoldable


textureAt :: Buffer -> Queue -> Maybe Texture
textureAt = Map.lookup


just :: Texture -> Queue
just = flip toDefault Map.empty


toDefault :: Texture -> Queue -> Queue
toDefault = toBuffer Default


toBuffer :: Buffer -> Texture -> Queue -> Queue
toBuffer = Map.insert