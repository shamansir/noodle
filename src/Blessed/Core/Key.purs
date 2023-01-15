module Blessed.Core.Key where

import Prelude

import Data.String as String
import Data.Array as Array



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