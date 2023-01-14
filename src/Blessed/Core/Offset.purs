module Blessed.Core.Offset where

import Prelude


import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA


import Blessed.Core.Coord (Coord)
import Blessed.Core.Coord as Coord


data Offset
    = Coord Coord
    | Center


instance Show Offset where
    show (Coord coord) = show coord
    show Center = "center"


instance EncodeJson Offset where
    encodeJson (Coord coord) = encodeJson coord
    encodeJson x = CA.encode CA.string $ show x


px ∷ Int → Offset
px = Coord.px >>> Coord


percents ∷ Number → Offset
percents = Coord.percents >>> Coord


calc :: Coord -> Offset
calc = Coord


center :: Offset
center = Center


render ∷ Offset → String
render Center = "center"
render (Coord coord) = Coord.render coord