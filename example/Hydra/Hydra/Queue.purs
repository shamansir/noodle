module Hydra.Queue where


import Prelude (flip, (>>>), class Show, show, ($), (<$>), (<>))

import Data.Maybe (Maybe)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String

import Hydra (Buffer(..), Texture)


newtype Queue =
    Queue (Buffer /-> Texture)


derive instance Newtype Queue _


empty :: Queue
empty = Queue Map.empty


isEmpty :: Queue -> Boolean
isEmpty = unwrap >>> Map.isEmpty


fromFoldable :: forall f. Foldable f => f (Buffer /\ Texture) -> Queue
fromFoldable = Map.fromFoldable >>> Queue


toUnfoldable :: forall f. Unfoldable f => Queue -> f (Buffer /\ Texture)
toUnfoldable = unwrap >>> Map.toUnfoldable


textureAt :: Buffer -> Queue -> Maybe Texture
textureAt buf = unwrap >>> Map.lookup buf


just :: Texture -> Queue
just = flip toDefault empty


toDefault :: Texture -> Queue -> Queue
toDefault = toBuffer Default


toBuffer :: Buffer -> Texture -> Queue -> Queue
toBuffer buf tex (Queue q) = Queue $ Map.insert buf tex q


atBuffer :: Buffer -> Texture -> Queue
atBuffer buf tex = toBuffer buf tex empty


instance Show Queue where
    show (Queue vals) =
        if Map.isEmpty vals
            then "<<empty>>"
            else String.joinWith "\n" $ showBuf <$> Map.toUnfoldable vals
        where showBuf (buf /\ tex) = show buf <> " :: " <> show tex