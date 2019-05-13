module Rpd.UUID
    ( UUID
    , new
    , toString
    , ToPatch(..), ToNode(..), ToInlet(..), ToOutlet(..), ToLink(..)
    ) where


import Prelude ((<#>), (<>), class Show, show, class Eq, class Ord)
import Effect (Effect)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd


newtype UUID = UUID String

-- foreign import new :: Unit -> Effect String


foreign import newAsString :: Effect String


newtype ToPatch = ToPatch UUID
newtype ToNode = ToNode UUID
newtype ToInlet = ToInlet UUID
newtype ToOutlet = ToOutlet UUID
newtype ToLink = ToLink UUID


new :: Effect UUID
new = newAsString <#> UUID


toString :: UUID -> String
toString (UUID uuid) = uuid


instance showUUID :: Show UUID where
    show (UUID uuid) = "{" <> uuid <> "}"


instance showToPatch :: Show ToPatch where
    show (ToPatch uuid) = "{P@" <> show uuid <> "}"

instance showToNode :: Show ToNode where
    show (ToNode uuid) = "{N@" <> show uuid <> "}"

instance showToInlet :: Show ToInlet where
    show (ToInlet uuid) = "{I@" <> show uuid <> "}"

instance showToOutlet :: Show ToOutlet where
    show (ToOutlet uuid) = "{O@" <> show uuid <> "}"

instance showToLink :: Show ToLink where
    show (ToLink uuid) = "{L@" <> show uuid <> "}"


derive instance genericUUID :: Generic UUID _
instance eqUUID :: Eq UUID where
  eq = GEq.genericEq
instance ordUUID :: Ord UUID where
  compare = GOrd.genericCompare


derive instance genericToPatch :: Generic ToPatch _
instance eqToPatch :: Eq ToPatch where
  eq = GEq.genericEq
instance ordToPatch :: Ord ToPatch where
  compare = GOrd.genericCompare


derive instance genericToNode :: Generic ToNode _
instance eqToNode :: Eq ToNode where
  eq = GEq.genericEq
instance ordToNode :: Ord ToNode where
  compare = GOrd.genericCompare


derive instance genericToInlet :: Generic ToInlet _
instance eqToInlet :: Eq ToInlet where
  eq = GEq.genericEq
instance ordToInlet :: Ord ToInlet where
  compare = GOrd.genericCompare


derive instance genericToOutlet :: Generic ToOutlet _
instance eqToOutlet :: Eq ToOutlet where
  eq = GEq.genericEq
instance ordToIutlet :: Ord ToOutlet where
  compare = GOrd.genericCompare


derive instance genericToLink :: Generic ToLink _
instance eqToLink :: Eq ToLink where
  eq = GEq.genericEq
instance ordToLink :: Ord ToLink where
  compare = GOrd.genericCompare
