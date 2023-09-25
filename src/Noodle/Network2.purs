module Noodle.Network2 where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)

import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Toolkit3 (Toolkit)
import Noodle.Stateful (class StatefulIx)
import Noodle.Stateful (get, set) as Stateful


data Network gstate (nodes :: Row Type) (instances :: Row Type) = Network (Toolkit gstate nodes) (Map Patch.Id (Patch gstate instances))


instance StatefulIx (Network gstate nodes instances) Patch.Id gstate where
    getIx :: Patch.Id -> Network gstate nodes instances -> Maybe gstate
    getIx id (Network _ ps) = Stateful.get <$> Map.lookup id ps
    setIx :: Patch.Id -> gstate -> Network gstate nodes instances -> Maybe (Network gstate nodes instances)
    setIx id state nw =
        if hasPatch id nw then
            Just $ withPatch id (Stateful.set state) nw -- Network tk $ Map.alter (map $ Stateful.set state) id ps
        else Nothing
    indices :: Network gstate nodes instances -> Set Patch.Id
    indices (Network _ ps) = Map.keys ps


init :: forall gstate nodes instances. Toolkit gstate nodes -> Network gstate nodes instances
init tk = Network tk Map.empty


patches :: forall gstate nodes instances. Network gstate nodes instances -> Map Patch.Id (Patch gstate instances)
patches (Network _ patches) = patches


patch :: forall gstate nodes instances. Patch.Id -> Network gstate nodes instances -> Maybe (Patch gstate instances)
patch id = Map.lookup id <<< patches


patchesCount :: forall gstate nodes instances. Network gstate nodes instances -> Int
patchesCount = patches >>> Map.size


hasPatch :: forall gstate nodes instances. Patch.Id -> Network gstate nodes instances -> Boolean
hasPatch id = patches >>> Map.member id


addPatch :: forall gstate nodes instances. Patch.Id -> Patch gstate instances -> Network gstate nodes instances -> Network gstate nodes instances
addPatch id patch (Network tk patches) =
    Network tk $ Map.insert id patch $ patches


withPatch :: forall gstate nodes instances. Patch.Id -> (Patch gstate instances -> Patch gstate instances) -> Network gstate nodes instances -> Network gstate nodes instances
withPatch patchId fn (Network tk patches) =
    Network tk $ Map.update (fn >>> Just) patchId $ patches