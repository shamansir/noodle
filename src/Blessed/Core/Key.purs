module Blessed.Core.Key where

import Prelude

import Data.String as String
import Data.Array as Array

import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA



newtype Key
    = Key String


only :: Key -> Array Key
only = Array.singleton


custom :: String -> Key
custom = Key


escape :: Key
escape = Key "escape"


enter :: Key
enter = Key "enter"


control :: Key -> Key
control (Key m) = Key $ "C-" <> m


alpha :: Char -> Key
alpha = Key <<< String.singleton <<< String.codePointFromChar


convertOne :: Key -> Json
convertOne (Key str) = CA.encode CA.string str


convertAll :: Array Key -> Array Json
convertAll = map convertOne