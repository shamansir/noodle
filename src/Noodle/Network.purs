module Noodle.Network where


import Prelude
import Effect (Effect)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence)

import Noodle.Node (Node, Link)
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Subpatch (Subpatch)


data Network node_state m d =
    Network
        (Patch.Id /-> Patch node_state m d)


empty :: forall node_state m d. Network node_state m d
empty = Network $ Map.empty


-- TODO: optics, State monad

patch :: forall node_state m d. Patch.Id  -> Network node_state m d -> Maybe (Patch node_state m d)
patch name (Network patches) = patches # Map.lookup name


patches :: forall node_state m d. Network node_state m d -> Array (Patch.Id /\ Patch node_state m d)
patches (Network patches) = patches # Map.toUnfoldableUnordered


addPatch :: forall node_state m d. Patch.Id /\ Patch node_state m d -> Network node_state m d -> Network node_state m d
addPatch (name /\ patch) (Network patches) = Network $ Map.insert name patch $ patches


removePatch :: forall node_state m d. Patch.Id -> Network node_state m d -> Network node_state m d
removePatch name (Network patches) = Network $ Map.delete name $ patches


withPatch :: forall node_state m d. Patch.Id -> (Patch node_state m d -> Patch node_state m d) -> Network node_state m d -> Network node_state m d
withPatch name f (Network patches) =
    Network
        $ Map.update (f >>> Just) name
        $ patches


withPatch' :: forall node_state m d. Patch.Id -> (Patch node_state m d -> Effect (Patch node_state m d)) -> Network node_state m d -> Effect (Network node_state m d)
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


addNode :: forall node_state m d. Patch.Id /\ Node.Id -> Node node_state m d -> Network node_state m d -> Network node_state m d
addNode (patch /\ nodeName) theNode =
    withPatch patch $ Patch.addNode nodeName theNode


-- addNodes :: forall d. String -> Array (String /\ Node d) -> Network d -> Network d
-- addNodes patch =


connect :: forall node_state m d. Patch.Id -> Patch.OutletPath -> Patch.InletPath -> Network node_state m d -> Effect (Network node_state m d)
connect patch outlet inlet = withPatch' patch $ Patch.connect outlet inlet


disconnect :: forall node_state m d. Patch.Id -> Patch.OutletPath -> Patch.InletPath -> Network node_state m d -> Effect (Network node_state m d)
disconnect patch outlet inlet = withPatch' patch $ Patch.disconnect outlet inlet