module Data.Lerp where


import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2


class Lerp x where
    -- lerp :: { from :: x, to :: x } -> Number -> x
    lerp :: x /\ x -> Number -> Maybe x


instance lerpNumber :: Lerp Number where
    lerp (from /\ to) amount =
        Just $ from + (to - from) * amount


instance lerpVec2 :: Lerp Vec2 where
    lerp (Vec2 fromX fromY /\ Vec2 toX toY) amount =
        Vec2
            <$> lerp (fromX /\ toX) amount
            <*> lerp (fromY /\ toY) amount
