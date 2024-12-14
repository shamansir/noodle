module Noodle.Network where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, lookup, insert, size, member, update) as Map

import Noodle.Id (PatchR)
import Noodle.Toolkit (Toolkit)
import Noodle.Patch (Patch)
import Noodle.Patch (id) as Patch
import Noodle.Toolkit (ToolkitKey)
import Noodle.Toolkit.Families (Families)


data Network (tk :: ToolkitKey) pstate (families :: Families) strepr chrepr m =
    Network
        (Toolkit tk families strepr chrepr m)
        (Map PatchR (Patch pstate families strepr chrepr m))


init :: forall tk ps fs sr cr m. Toolkit tk fs sr cr m -> Network tk ps fs sr cr m
init tk = Network tk Map.empty


patches :: forall tk ps fs sr cr m. Network tk ps fs sr cr m -> Map PatchR (Patch ps fs sr cr m)
patches (Network _ patches) = patches


patch :: forall tk ps fs sr cr m. PatchR -> Network tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch id = Map.lookup id <<< patches


patchesCount :: forall tk ps fs sr cr m. Network tk ps fs sr cr m  -> Int
patchesCount = patches >>> Map.size


hasPatch :: forall tk ps fs sr cr m. PatchR -> Network tk ps fs sr cr m  -> Boolean
hasPatch id = patches >>> Map.member id


toolkit :: forall tk ps fs sr cr m. Network tk ps fs sr cr m -> Toolkit tk fs sr cr m
toolkit (Network tk _) = tk


addPatch
    :: forall tk ps fs sr cr m
    .  Patch ps fs sr cr m
    -> Network tk ps fs sr cr m
    -> Network tk ps fs sr cr m
addPatch patch (Network tk patches) =
    Network tk $ Map.insert (Patch.id patch) patch $ patches


withPatch
    :: forall tk ps fs sr cr m
    .  PatchR
    -> (Patch ps fs sr cr m -> Patch ps fs sr cr m )
    -> Network tk ps fs sr cr m
    -> Network tk ps fs sr cr m
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches