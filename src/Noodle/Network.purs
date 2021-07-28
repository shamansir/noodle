module Noodle.Network where


import Prelude
import Effect (Effect)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence)

import Noodle.Node (Node, Link)
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Subpatch (Subpatch)


data Network d =
    Network
        (String /-> Patch d)


empty :: forall d. Network d
empty = Network $ Map.empty


-- TODO: optics, State monad

patch :: forall d. String -> Network d -> Maybe (Patch d)
patch name (Network patches) = patches # Map.lookup name


patches :: forall d. Network d -> Array (String /\ Patch d)
patches (Network patches) = patches # Map.toUnfoldable


addPatch :: forall d. String /\ Patch d -> Network d -> Network d
addPatch (name /\ patch) (Network patches) = Network $ Map.insert name patch $ patches


removePatch :: forall d. String -> Network d -> Network d
removePatch name (Network patches) = Network $ Map.delete name $ patches


withPatch :: forall d. String -> (Patch d -> Patch d) -> Network d -> Network d
withPatch name f (Network patches) =
    Network
        $ Map.update (f >>> Just) name
        $ patches


withPatch' :: forall d. String -> (Patch d -> Effect (Patch d)) -> Network d -> Effect (Network d)
withPatch' name f nw@(Network patches) =
    do
        maybeNextPatch <- sequence (patch name nw <#> f)
        case maybeNextPatch of
            Just nextPatch ->
                pure
                    $ Network
                    $ Map.update (const $ Just nextPatch) name
                    $ patches
            Nothing ->
                pure nw


addNode :: forall d. String /\ String -> Node d -> Network d -> Network d
addNode (patch /\ nodeName) theNode =
    withPatch patch $ Patch.addNode nodeName theNode


-- addNodes :: forall d. String -> Array (String /\ Node d) -> Network d -> Network d
-- addNodes patch =


connect :: forall d. String -> Patch.OutletPath -> Patch.InletPath -> Network d -> Effect (Network d)
connect patch outlet inlet = withPatch' patch $ Patch.connect outlet inlet


disconnect :: forall d. String -> Patch.OutletPath -> Patch.InletPath -> Network d -> Effect (Network d)
disconnect patch outlet inlet = withPatch' patch $ Patch.disconnect outlet inlet