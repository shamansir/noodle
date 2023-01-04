module Data.UUID
    ( UUID
    , generate
    , toString
    ) where

import Prelude

import Effect (Effect)


newtype UUID = UUID String
derive newtype instance eqUUID :: Eq UUID
derive newtype instance ordUUID :: Ord UUID
derive newtype instance showUUID :: Show UUID


foreign import generate_ :: Effect String


generate :: Effect UUID
generate = UUID <$> generate_


toString :: UUID -> String
toString (UUID s) = s