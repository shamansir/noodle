module Data.Layout where


import Data.Maybe (Maybe)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)


import Data.Vec2 (Pos, Size)


import Data.Tuple.Nested ((/\), type (/\))


class FoldableWithIndex Int f <= Layout f where
    size :: forall a. f a -> Size
    empty :: forall a. Size -> f a
    pack :: forall a. a -> Size -> f a -> Maybe (f a)
    packAt :: forall a. a -> Pos -> Size -> f a -> Maybe (f a)
    toFoldable :: forall m a. Foldable m => f a -> m (a /\ Pos /\ Size)
    find :: forall a. a -> f a -> Maybe (Pos /\ Size)
    sample :: forall a. Pos -> f a -> Maybe (a /\ Pos /\ Size)
    reflow :: forall a. Size -> f a -> Maybe (f a)