module Noodle.Network2 where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))


import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Toolkit3 (Toolkit)


data Network (nodes :: Row Type) (instances :: Row Type) = Network (Toolkit Unit nodes) (Map Patch.Id (Patch Unit instances))



init :: forall nodes instances. Toolkit Unit nodes -> Network nodes instances
init tk = Network tk Map.empty


patches :: forall nodes instances. Network nodes instances -> Map Patch.Id (Patch Unit instances)
patches (Network _ patches) = patches


patch :: forall nodes instances. Patch.Id -> Network nodes instances -> Maybe (Patch Unit instances)
patch id = Map.lookup id <<< patches


addPatch :: forall nodes instances. Patch.Id -> Patch Unit instances -> Network nodes instances -> Network nodes instances
addPatch id patch (Network tk patches) =
    Network tk $ Map.insert id patch $ patches


withPatch :: forall nodes instances. Patch.Id -> (Patch Unit instances -> Patch Unit instances) -> Network nodes instances -> Network nodes instances
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches