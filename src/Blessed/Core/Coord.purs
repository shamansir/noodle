module Blessed.Core.Coord where

import Prelude
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA


data Coord
    = Px Int
    | Percent Number
    | Sum Coord Coord
    | Sub Coord Coord


instance Show Coord where
    show (Px px) = show px
    show (Percent pc) = show pc <> "%"
    show (Sum cA cB) = show cA <> "+" <> show cB
    show (Sub cA cB) = show cA <> "-" <> show cB


instance EncodeJson Coord where
    encodeJson coord = CA.encode CA.string $ show coord


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
