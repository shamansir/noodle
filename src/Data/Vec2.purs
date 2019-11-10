module Data.Vec2 where

import Prelude

import Math (atan2, sqrt, pow)
import Data.Tuple.Nested ((/\), type (/\))


data Vec2 = Vec2 Number Number


instance showVec2 :: Show Vec2 where
    show (Vec2 x y) = "vec(" <> show x <> "," <> show y <> ")"


arrow :: Vec2 /\ Vec2 -> { angle :: Number, length :: Number }
arrow (Vec2 fromX fromY /\ Vec2 toX toY) =
    let
        Vec2 x y = Vec2 (toX - fromX) (toY - fromY)
        angle = atan2 y x
        length = sqrt $ pow x 2.0 + pow y 2.0
    in
        { angle, length }
