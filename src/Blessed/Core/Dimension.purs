module Blessed.Core.Dimension where

import Prelude

import Blessed.Core.Coord (Coord)
import Blessed.Core.Coord as Coord


type Dimension = Coord


infixl 6 Coord.sum as <+>
infixl 6 Coord.sub as <->


px ∷ Int → Dimension
px = Coord.px


percents ∷ Number → Dimension
percents = Coord.percents


render ∷ Dimension → String
render = Coord.render