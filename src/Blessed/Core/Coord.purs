module Blessed.Core.Coord where

import Prelude


data Coord
    = Px Int
    | Percent Number
    | Sum Coord Coord
    | Sub Coord Coord


px :: Int -> Coord
px = Px


percents :: Number -> Coord
percents = Percent


sum :: Coord -> Coord -> Coord
sum = Sum


sub :: Coord -> Coord -> Coord
sub = Sub


infixl 6 add as <+>
infixl 6 sub as <->


render :: Coord -> String
render (Px i) = show i
render (Percent n) = show n <> "%"
render (Sum a b) = render a <> "+" <> render b
render (Sub a b) = render a <> "-" <> render b


-- instance Semiring Coord where
--     add = sum
--     zero = Px 0
--     one = Px 1
--     mul =
