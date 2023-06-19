module Data.FromToFile where

import Prelude (show, identity)

import Data.Maybe (Maybe)

import Data.String.Read (class Read, read)


class Encode a where
    encode :: a -> String


class Decode a where
    decode :: String -> Maybe a


instance Encode Number where
    encode = show


instance Encode Int where
    encode = show


instance Read a => Decode a where
    decode = read


instance Encode String where
    encode = identity
