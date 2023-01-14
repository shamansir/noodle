module Blessed.Core.Key where

import Prelude

import Data.String as String



newtype Key
    = Key String


custom :: String -> Key
custom = Key


escape :: Key
escape = Key "escape"


control :: Key -> Key
control (Key m) = Key $ "C-" <> m


alpha :: Char -> Key
alpha = Key <<< String.singleton <<< String.codePointFromChar