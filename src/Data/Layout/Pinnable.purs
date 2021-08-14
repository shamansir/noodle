module Data.Layout.Pinnable where


import Data.Layout (class IsLayout)
import Data.Vec2 (Pos, Size)
import Data.Maybe (Maybe)


class IsLayout l <= IsPinnableLayout l where
    pin :: forall a. a -> Pos -> Size -> l a -> Maybe (l a)


-- other implementation of reflow?