module Blessed.Command where

import Foreign.JSON
import Foreign (Foreign)


type Command =
    { taget :: Int, cmd :: String, args :: Array Foreign }