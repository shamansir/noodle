module Blessed.Core.Style where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA

import Data.Maybe (Maybe(..))

import Blessed.Core.Color (Color)
import Blessed.Core.Border (Border)
import Blessed.Core.Border as Border


type StyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: Maybe Border
    )
type Style =
    Record (StyleRow ())



default :: Style
default =
    { border : Nothing
    , fg : "none"
    , bg : "none"
    }