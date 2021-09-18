module App.Layout where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr, class Foldable)
import Data.Unfoldable (unfoldr, class Unfoldable)
import Data.FoldableWithIndex (class FoldableWithIndex)


import Data.Vec2 (Pos, Size, (<+>))
import Data.Array ((:))
import Data.Array as Array


import Data.Tuple.Nested ((/\), type (/\))


class {-FoldableWithIndex Pos l <=-} IsLayout l where
    size :: forall a. l a -> Size
    container :: forall a. Size -> l a
    --toFoldable :: forall f a. Foldable f => l a -> f (a /\ Pos /\ Size)
    --toUnfoldable :: forall f a. Unfoldable f => l a -> f (a /\ Pos /\ Size)
    --unfold :: forall a. l a -> Array (a /\ Pos /\ Size)
    fold :: forall a k. ((a /\ Pos /\ Size) -> k -> k) -> k -> l a -> k
    find :: forall a. a -> l a -> Maybe (Pos /\ Size)
    sample :: forall a. Pos -> l a -> Maybe (a /\ Pos /\ Size)
    remove :: forall a. Eq a => a -> l a -> Maybe (l a)
    -- reflow :: forall a. Size -> l a -> Maybe (l a)


class IsLayout l <= IsAutoLayout l where
    pack :: forall a. a -> Size -> l a -> Maybe (l a)
    --packMany ::


class IsLayout l <= IsPinningLayout l where
    pin :: forall a. a -> Pos -> Size -> l a -> l a
    -- unpin == remove


-- TODO: what are:
--   * both Pin and Auto layouts
--   * layout that saves order of adding `IsLayout (Timestamp /\ a)`
--   * layout like `elm-ui` (may be just `IsLayout` with own packing)


-- TODO: scale

sqContainer :: forall l a. IsLayout l => Number -> l a
sqContainer n = container $ n <+> n


count :: forall l a. IsLayout l => l a -> Int
count = (toUnfoldable :: l a -> Array (a /\ Pos /\ Size)) >>> Array.length


toUnfoldable :: forall f l a. Unfoldable f => IsLayout l => l a -> f (a /\ Pos /\ Size)
toUnfoldable = fold (:) [] >>> Array.toUnfoldable


packOrDrop :: forall l a. IsAutoLayout l => a -> Size -> l a -> l a
packOrDrop a size dst = fromMaybe dst $ pack a size dst


reflow :: forall l a. IsAutoLayout l => Size -> l a -> Maybe (l a)
reflow size layout =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            dst >>= pack a itemSize
        )
        (Just $ container size)
        (toUnfoldable layout :: Array (a /\ Pos /\ Size))


reflowOrDrop :: forall l a. IsAutoLayout l => Size -> l a -> l a
reflowOrDrop size layout =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            packOrDrop a itemSize dst
        )
        (container size)
        (toUnfoldable layout :: Array (a /\ Pos /\ Size))


packMany :: forall f l a. IsAutoLayout l => Foldable f => f (a /\ Size) -> l a -> Maybe (l a)
packMany source layout =
    foldr
        (\(a /\ itemSize) dst -> dst >>= pack a itemSize)
        (Just layout)
        source


packOrDropMany :: forall f l a. IsAutoLayout l => Foldable f => f (a /\ Size) -> l a -> l a
packOrDropMany source layout =
    foldr
        (\(a /\ itemSize) dst -> packOrDrop a itemSize dst)
        layout
        source



-- packOrDrop

-- packOrDropMany


-- getByPos