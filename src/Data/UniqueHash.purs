module Data.UniqueHash
    ( UniqueHash
    , generate
    , toString
    ) where

import Prelude

import Effect (Effect)


newtype UniqueHash = UniqueHash String
derive newtype instance eqUniqueHash :: Eq UniqueHash
derive newtype instance ordUniqueHash :: Ord UniqueHash
derive newtype instance showUniqueHash :: Show UniqueHash


foreign import generate_ :: Effect String


generate :: Effect UniqueHash
generate = UniqueHash <$> generate_


toString :: UniqueHash -> String
toString (UniqueHash s) = s