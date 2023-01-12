module Blessed.Core.Border where

import Prelude


import Blessed.Core.Color (Color)

data BorderType
    = Line


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
