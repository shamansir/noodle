module Prev.Noodle.Network where


import Prelude
import Effect (Effect)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence)
import Data.Exists (Exists, mkExists, runExists)
import Unsafe.Coerce (unsafeCoerce)

import Prev.Noodle.Node (Node, Link)
import Prev.Noodle.Node as Node
import Prev.Noodle.Patch (Patch)
import Prev.Noodle.Patch as Patch
import Prev.Noodle.Subpatch (Subpatch)


-- data NodeS d state = NodeS state (state -> Node state d)
data PatchS d state = PatchS (Patch state d)
type PatchE d = Exists (PatchS d)


data Network d =
    Network
        (Patch.Id /-> PatchE d)


empty :: forall d. Network d
empty = Network $ Map.empty


-- TODO: optics, State monad

patch :: forall state d. Patch.Id  -> Network d -> Maybe (Patch state d)
patch name (Network patches) = patches # Map.lookup name # map unwrapPatch


-- patches :: forall state d. Network d -> Array (Patch.Id /\ Patch state d)
patches :: forall d. Network d -> Array (Patch.Id /\ PatchE d)
patches (Network patches) = patches # Map.toUnfoldableUnordered


addPatch :: forall state d. Patch.Id /\ Patch state d -> Network d -> Network d
addPatch (name /\ patch) (Network patches) = Network $ Map.insert name (wrapPatch patch) $ patches


removePatch :: forall d. Patch.Id -> Network d -> Network d
removePatch name (Network patches) = Network $ Map.delete name $ patches


withPatch :: forall d. Patch.Id -> (forall state. Patch state d -> Patch state d) -> Network d -> Network d
withPatch name f (Network patches) =
    Network
        $ Map.update (unwrapPatch >>> f >>> wrapPatch >>> Just) name
        $ patches


withPatch' :: forall d. Patch.Id -> (forall state. Patch state d -> Effect (Patch state d)) -> Network d -> Effect (Network d)
withPatch' name f nw@(Network patches) =
    do
        maybeNextPatch <- sequence (patch name nw <#> f <#> map wrapPatch)
        case maybeNextPatch of
            Just nextPatch ->
                pure
                    $ Network
                    $ Map.update (const $ Just nextPatch) name
                    $ patches
            Nothing ->
                pure nw


addNode :: forall state d. Patch.Id /\ Node.Id -> Node state d -> Network d -> Network d
addNode (patch /\ nodeName) theNode =
    -- withPatch patch $ Patch.addNode nodeName theNode
    withPatch patch (Patch.addNode nodeName theNode)


-- addNodes :: forall d. String -> Array (String /\ Node d) -> Network d -> Network d
-- addNodes patch =


connect :: forall d. Patch.Id -> Patch.OutletPath -> Patch.InletPath -> Network d -> Effect (Network d)
-- connect patch outlet inlet = withPatch' patch $ Patch.connect outlet inlet
connect patch outlet inlet = withPatch' patch (Patch.connect outlet inlet)


disconnect :: forall d. Patch.Id -> Patch.OutletPath -> Patch.InletPath -> Network d -> Effect (Network d)
-- disconnect patch outlet inlet = withPatch' patch $ Patch.disconnect outlet inlet
disconnect patch outlet inlet = withPatch' patch (Patch.disconnect outlet inlet)


unwrapPatch :: forall state d. PatchE d -> Patch state d
unwrapPatch = runExists (\(PatchS patch) -> unsafeCoerce patch)


wrapPatch :: forall state d. Patch state d -> PatchE d
wrapPatch = mkExists <<< PatchS