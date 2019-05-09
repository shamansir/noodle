module Rpd.UUID
    ( UUID
    , new
    , toString
    , ToPatch, ToNode, ToInlet, ToOutlet, ToLink
    ) where


import Prelude ((==), (<#>), (<>), class Show, show, class Eq, eq)
import Effect (Effect)


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


instance eqUUID :: Eq UUID where
    eq (UUID uuidA) (UUID uuidB) = uuidA == uuidB



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
