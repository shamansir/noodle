module Blessed.Core.Flex where

import Prelude


import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA

data Flex
    = Shrink
    | Flex
    | Grow
    | Width
    | Height


instance EncodeJson Flex where
    encodeJson = CA.encode CA.string <<< show


instance Show Flex where
    show Shrink = "shrink"
    show Flex = "flex"
    show Grow = "grow"
    show Width = "width"
    show Height = "height"
