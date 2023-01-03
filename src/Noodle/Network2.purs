module Noodle.Network2 where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))


import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Toolkit3 (Toolkit)


data Network gstate (nodes :: Row Type) (instances :: Row Type) = Network (Toolkit gstate nodes) (Map Patch.Id (Patch gstate instances))



init :: forall gstate nodes instances. Toolkit gstate nodes -> Network gstate nodes instances
init tk = Network tk Map.empty


patches :: forall gstate nodes instances. Network gstate nodes instances -> Map Patch.Id (Patch gstate instances)
patches (Network _ patches) = patches


patch :: forall gstate nodes instances. Patch.Id -> Network gstate nodes instances -> Maybe (Patch gstate instances)
patch id = Map.lookup id <<< patches


addPatch :: forall gstate nodes instances. Patch.Id -> Patch gstate instances -> Network gstate nodes instances -> Network gstate nodes instances
addPatch id patch (Network tk patches) =
    Network tk $ Map.insert id patch $ patches


withPatch :: forall gstate nodes instances. Patch.Id -> (Patch gstate instances -> Patch gstate instances) -> Network gstate nodes instances -> Network gstate nodes instances
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches