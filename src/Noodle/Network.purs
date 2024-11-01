module Noodle.Network where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, lookup, insert, size, member, update) as Map

import Noodle.Id (PatchR)
import Noodle.Toolkit (Toolkit)
import Noodle.Patch (Patch)
import Noodle.Toolkit.Families (Families)


data Network pstate (families :: Families) repr m = Network (Toolkit families repr m) (Map PatchR (Patch pstate families repr m))


init :: forall pstate families repr m. Toolkit families repr m -> Network pstate families repr m
init tk = Network tk Map.empty


patches :: forall pstate families repr m. Network pstate families repr m -> Map PatchR (Patch pstate families repr m)
patches (Network _ patches) = patches


patch :: forall pstate families repr m. PatchR -> Network pstate families repr m -> Maybe (Patch pstate families repr m)
patch id = Map.lookup id <<< patches


patchesCount :: forall pstate families repr m. Network pstate families repr m  -> Int
patchesCount = patches >>> Map.size


hasPatch :: forall pstate families repr m. PatchR -> Network pstate families repr m  -> Boolean
hasPatch id = patches >>> Map.member id


addPatch
    :: forall pstate families repr m
    .  PatchR
    -> Patch pstate families repr m
    -> Network pstate families repr m
    -> Network pstate families repr m
addPatch id patch (Network tk patches) =
    Network tk $ Map.insert id patch $ patches


withPatch
    :: forall pstate families repr m
    .  PatchR
    -> (Patch pstate families repr m -> Patch pstate families repr m )
    -> Network pstate families repr m
    -> Network pstate families repr m
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches