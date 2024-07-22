module Noodle.Fn.Protocol
  ( Protocol
  , make
  )
  where


import Prelude (unit, (<#>), map)

import Prim.RowList as RL

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.SProxy (reflect')

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InputR, OutputR)

import Noodle.Fn.Raw.Protocol as Raw
import Noodle.Fn.Tracker (Tracker)

import Data.Repr (Repr(..), class FromReprRow)
import Data.Repr (fromMap) as Repr


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InputR repr
    -> Map OutputR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make


getRecInputs :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Protocol state is os repr -> Effect (Record is)
getRecInputs p = p.getInputs unit <#> Tuple.snd <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap


getRecOutputs :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Protocol state is os repr -> Effect (Record os)
getRecOutputs p = p.getOutputs unit <#> Tuple.snd <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap