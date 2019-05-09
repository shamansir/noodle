module Rpd.UUID
    ( UUID
    , new
    , toString
    , ToPatch, ToNode, ToInlet, ToOutlet, ToLink
    ) where


import Prelude ((<#>), (<>), class Show)
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
