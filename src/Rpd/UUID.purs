module Rpd.UUID
    ( UUID
    , new
    , toString
    ) where


import Prelude ((<#>), (<>), class Show, class Eq, class Ord)
import Effect (Effect)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd


newtype UUID = UUID String

-- foreign import new :: Unit -> Effect String


foreign import newAsString :: Effect String


new :: Effect UUID
new = newAsString <#> UUID


toString :: UUID -> String
toString (UUID uuid) = uuid


instance showUUID :: Show UUID where
    show (UUID uuid) = "{" <> uuid <> "}"


derive instance genericUUID :: Generic UUID _
instance eqUUID :: Eq UUID where
  eq = GEq.genericEq
instance ordUUID :: Ord UUID where
  compare = GOrd.genericCompare
