module Blessed.Core.Offset where

import Prelude


import Blessed.Core.Coord (Coord)
import Blessed.Core.Coord as Coord


data Offset
    = Coord Coord
    | Center


px ∷ Int → Offset
px = Coord.px >>> Coord


percents ∷ Number → Offset
percents = Coord.percents >>> Coord


calc :: Coord -> Offset
calc = Coord


render ∷ Offset → String
render Center = "center"
render (Coord coord) = Coord.render coord