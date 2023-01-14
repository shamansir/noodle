module Blessed.Core.Border where

import Prelude


import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA


import Blessed.Core.Color (Color)

data BorderType
    = Line


instance Show BorderType where
    show Line = "line"


instance EncodeJson BorderType where
    encodeJson Line = CA.encode CA.string "line"


type BorderRow (r :: Row Type) =
    ( type :: BorderType
    , fg :: Color
    , bg :: Color
    )
type Border = Record (BorderRow ())


default :: Border
default =
    { type : Line
    , fg : "none"
    , bg : "none"
    }
