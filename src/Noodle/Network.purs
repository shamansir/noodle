module Noodle.Network where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, lookup, insert, size, member, update) as Map

import Noodle.Id (PatchR)
import Noodle.Toolkit (Toolkit)
import Noodle.Patch (Patch)
import Noodle.Patch (id) as Patch
import Noodle.Toolkit.Families (Families)


data Network pstate (families :: Families) repr m =
    Network
        (Toolkit families repr m)
        (Map PatchR (Patch pstate families repr m))


init :: forall s fs r m. Toolkit fs r m -> Network s fs r m
init tk = Network tk Map.empty


patches :: forall s fs r m. Network s fs r m -> Map PatchR (Patch s fs r m)
patches (Network _ patches) = patches


patch :: forall s fs r m. PatchR -> Network s fs r m -> Maybe (Patch s fs r m)
patch id = Map.lookup id <<< patches


patchesCount :: forall s fs r m. Network s fs r m  -> Int
patchesCount = patches >>> Map.size


hasPatch :: forall s fs r m. PatchR -> Network s fs r m  -> Boolean
hasPatch id = patches >>> Map.member id


toolkit :: forall s fs r m. Network s fs r m -> Toolkit fs r m
toolkit (Network tk _) = tk


addPatch
    :: forall s fs r m
    .  Patch s fs r m
    -> Network s fs r m
    -> Network s fs r m
addPatch patch (Network tk patches) =
    Network tk $ Map.insert (Patch.id patch) patch $ patches


withPatch
    :: forall s fs r m
    .  PatchR
    -> (Patch s fs r m -> Patch s fs r m )
    -> Network s fs r m
    -> Network s fs r m
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches