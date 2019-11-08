module Data.Lerp where


import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


class Lerp x where
    -- lerp :: { from :: x, to :: x } -> Number -> x
    lerp :: x /\ x -> Number -> Maybe x


instance lerpNumber :: Lerp Number where
    lerp (from /\ to) amount =
        Just $ from + (to - from) * amount
