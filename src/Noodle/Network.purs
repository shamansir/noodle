module Noodle.Network where


import Prelude (($), (#))

import Noodle.Node (Node, Link)
import Noodle.Patch (Patch)
import Noodle.Subpatch (Subpatch)

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))


data Network d =
    Network
        (String /-> Patch d)


empty :: forall d. Network d
empty = Network $ Map.empty


-- TODO: optics

patch :: forall d. String -> Network d -> Maybe (Patch d)
patch name (Network patches) = patches # Map.lookup name


patches :: forall d. Network d -> Array (String /\ Patch d)
patches (Network patches) = patches # Map.toUnfoldable


addPatch :: forall d. String /\ Patch d -> Network d -> Network d
addPatch (name /\ patch) (Network patches) = Network $ Map.insert name patch $ patches


removePatch :: forall d. String -> Network d -> Network d
removePatch name (Network patches) = Network $ Map.delete name $ patches