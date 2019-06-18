module UUID
    ( UUID
    , new
    , toString
    ) where


import Prelude
import Effect (Effect)


foreign import newAsString :: Effect String


newtype UUID = UUID String

derive instance eqUuid :: Eq UUID
derive instance ordUuid :: Ord UUID


new :: Effect UUID
new = newAsString <#> UUID


toString :: UUID -> String
toString (UUID uuid) = uuid


instance showUUID :: Show UUID where
  show (UUID uuid) = "{" <> uuid <> "}"
