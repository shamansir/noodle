module Blessed.Core.Coord where

import Prelude
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA


data Coord
    = Px Int
    | Percent Number
    | Sum Coord Coord
    | Sub Coord Coord
    | Center


instance Show Coord where
    show (Px px) = show px
    show (Percent pc) = show pc <> "%"
    show (Sum cA cB) = show cA <> "+" <> show cB
    show (Sub cA cB) = show cA <> "-" <> show cB
    show (Center) = "center"


instance EncodeJson Coord where
    encodeJson (Px px) = CA.encode CA.int px
    encodeJson coord = CA.encode CA.string $ show coord


px :: Int -> Coord
px = Px


percents :: Number -> Coord
percents = Percent


-- TODO: Semiring, Ring


sum :: Coord -> Coord -> Coord
sum = Sum


sub :: Coord -> Coord -> Coord
sub = Sub


center :: Coord
center = Center


infixl 6 sum as <+> -- FIXME: implement Semiring ?
infixl 6 sub as <-> -- FIXME: implement Ring ?


render :: Coord -> String
render (Px i) = show i
render (Percent n) = show n <> "%"
render (Sum a b) = render a <> "+" <> render b
render (Sub a b) = render a <> "-" <> render b
render (Center) = "center"


-- instance Semiring Coord where
--     add = sum
--     zero = Px 0
--     one = Px 1
--     mul =
