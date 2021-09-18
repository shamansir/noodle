module App.Layout where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr, class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)


import Data.Vec2 (Pos, Size)


import Data.Tuple.Nested ((/\), type (/\))


class {-FoldableWithIndex Pos l <=-} IsLayout l where
    size :: forall a. l a -> Size
    container :: forall a. Size -> l a
    toFoldable :: forall f a. Foldable f => l a -> f (a /\ Pos /\ Size)
    -- toUnfoldable ::
    find :: forall a. a -> l a -> Maybe (Pos /\ Size)
    sample :: forall a. Pos -> l a -> Maybe (a /\ Pos /\ Size)
    remove :: forall a. Eq a => a -> l a -> l a
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


packOrDrop :: forall l a. IsAutoLayout l => a -> Size -> l a -> l a
packOrDrop a size dst = fromMaybe dst $ pack a size dst


reflow :: forall l a. IsAutoLayout l => Size -> l a -> Maybe (l a)
reflow size layout =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            dst >>= pack a itemSize
        )
        (Just $ container size)
        (toFoldable layout :: Array (a /\ Pos /\ Size))


reflowOrDrop :: forall l a. IsAutoLayout l => Size -> l a -> l a
reflowOrDrop size layout =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            packOrDrop a itemSize dst
        )
        (container size)
        (toFoldable layout :: Array (a /\ Pos /\ Size))


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