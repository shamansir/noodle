module Noodle.Network2 where

import Prelude

import Data.Map (Map)
import Data.Map as Map


import Noodle.Patch2 (Patch)
import Noodle.Patch2 as Patch
import Noodle.Toolkit3 (Toolkit)


data Network (nodes :: Row Type) (instances :: Row Type) = Network (Toolkit nodes) (Map String (Patch instances))



init :: forall nodes instances. Toolkit nodes -> Network nodes instances
init tk = Network tk Map.empty