module Blessed.Core.Dimension where

import Prelude


import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA


import Blessed.Core.Coord (Coord)
import Blessed.Core.Coord as Coord


data Dimension
    = Coord Coord
    | Half
    | Shrink


instance Show Dimension where
    show (Coord coord) = show coord
    show Half = "half"
    show Shrink = "shrink"


instance EncodeJson Dimension where
    encodeJson (Coord coord) = encodeJson coord
    encodeJson x = CA.encode CA.string $ show x


px ∷ Int → Dimension
px = Coord.px >>> Coord


percents ∷ Number → Dimension
percents = Coord.percents >>> Coord


calc :: Coord -> Dimension
calc = Coord


half :: Dimension
half = Half


shrink :: Dimension
shrink = Shrink


render ∷ Dimension → String
render Half = "half"
render Shrink = "shrink"
render (Coord coord) = Coord.render coord