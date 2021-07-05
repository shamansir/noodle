module Noodle.Network where


import Prelude (($), (#))

import Noodle.Node (Node, Link)
import Noodle.Patch (Patch)
import Noodle.Subpatch (Subpatch)

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))


data Network d a =
    Network
        (String /-> Patch d a)


empty :: forall d a. Network d a
empty = Network $ Map.empty


-- TODO: optics

patch :: forall d a. String -> Network d a -> Maybe (Patch d a)
patch name (Network patches) = patches # Map.lookup name


patches :: forall d a. Network d a -> Array (String /\ Patch d a)
patches (Network patches) = patches # Map.toUnfoldable


addPatch :: forall d a. String /\ Patch d a -> Network d a -> Network d a
addPatch (name /\ patch) (Network patches) = Network $ Map.insert name patch $ patches


removePatch :: forall d a. String -> Network d a -> Network d a
removePatch name (Network patches) = Network $ Map.delete name $ patches