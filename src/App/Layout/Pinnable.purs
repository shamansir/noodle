module App.Layout.Pinnable where


-- import Prelude ((>>=), ($))
import Data.Eq (class Eq)

import Data.Vec2 (Pos, Size)
-- import Data.Maybe (Maybe(..))
-- import Data.Foldable (foldr)
-- import Data.Tuple.Nested ((/\), type (/\))

import App.Layout


class IsLayout l <= IsPinnableLayout l where
    pin :: forall a. a -> Pos -> Size -> l a -> l a


unpin :: forall l a. IsPinnableLayout l => Eq a => a -> l a -> l a
unpin = remove


-- No need to reflow!


-- pinNowhere :: forall l a. IsPinnableLayout l => a -> Size -> l a -> l a
-- pinNowhere