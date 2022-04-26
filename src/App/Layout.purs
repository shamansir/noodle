module App.Layout where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr, class Foldable)
import Data.Unfoldable (unfoldr, class Unfoldable)
import Data.FoldableWithIndex (class FoldableWithIndex)


import Data.Vec2 (Pos, Size, (<+>))
import Data.Array ((:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List


import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))


class {-FoldableWithIndex Pos l <=-} IsLayout l where
    --toFoldable :: forall f a. Foldable f => l a -> f (a /\ Pos /\ Size)
    --toUnfoldable :: forall f a. Unfoldable f => l a -> f (a /\ Pos /\ Size)
    --unfold :: forall a. l a -> Array (a /\ Pos /\ Size)
    fold :: forall a k. ((a /\ Pos /\ Size) -> k -> k) -> k -> l a -> k
    find :: forall a. Eq a => a -> l a -> Maybe (Pos /\ Size)
    sample :: forall a. Pos -> l a -> Maybe (a /\ Pos /\ Size)
    --remove :: forall a. Eq a => a -> l a -> Maybe (l a)
    -- reflow :: forall a. Size -> l a -> Maybe (l a)

class IsLayout l <= IsSizedLayout l where
    size :: forall a. l a -> Size


class IsSizedLayout l <= IsContainerLayout l where
    container :: forall a. Size -> l a

-- TODO: join `IsContainerLayout`` with `IsSizedLayout` back, because `container _ = empty` means can have any size and still work

class IsContainerLayout l <= IsAutoLayout l where
    pack :: forall a. a -> Size -> l a -> Maybe (l a)
    --packMany ::


class IsContainerLayout l <= IsPinningLayout l where
    pin :: forall a. a -> Pos -> Size -> l a -> l a
    unpin :: forall a. Eq a => a -> l a -> l a
    --unpin :: forall a. Eq a => a -> l a -> Maybe (l a)
    -- unpin == remove


-- TODO: what are:
--   * both Pin and Auto layouts;
--   * layout of (Maybe items)
--   * layout that saves order of adding `IsLayout (Timestamp /\ a)`;
--   * layout like `elm-ui` (may be just `IsLayout` with own packing); (Flex Layout)
--   * layered layouts;


-- TODO: scale

sqContainer :: forall l a. IsContainerLayout l => Number -> l a
sqContainer n = container $ n <+> n


count :: forall l a. IsLayout l => l a -> Int
count = toArray >>> Array.length


toList :: forall l a. IsLayout l => l a -> List (a /\ Pos /\ Size)
toList = fold Cons Nil


toArray :: forall l a. IsLayout l => l a -> Array (a /\ Pos /\ Size)
toArray = fold (:) []


-- toList ::


toUnfoldable :: forall f l a. Unfoldable f => IsLayout l => l a -> f (a /\ Pos /\ Size)
toUnfoldable = toArray >>> Array.toUnfoldable


atPos :: forall l a. IsLayout l => Pos -> l a -> Maybe a
atPos pos layout = Tuple.fst <$> sample pos layout


atPos' :: forall l a. IsLayout l => Pos -> l a -> Maybe (a /\ Pos)
atPos' pos layout = (\(a /\ pos' /\ _) -> a /\ pos') <$> sample pos layout


atPos'' :: forall l a. IsLayout l => Pos -> l a -> Maybe (a /\ Size)
atPos'' pos layout = (\(a /\ _ /\ size) -> a /\ size) <$> sample pos layout


packOrDrop :: forall l a. IsAutoLayout l => a -> Size -> l a -> l a
packOrDrop a size dst = fromMaybe dst $ pack a size dst


reflow :: forall l a. IsAutoLayout l => Size -> l a -> Maybe (l a)
reflow size =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            dst >>= pack a itemSize
        )
        (Just $ container size)
        <<< toArray


reflowOrDrop :: forall l a. IsAutoLayout l => Size -> l a -> l a
reflowOrDrop size =
    foldr
        (\(a /\ (_ /\ itemSize)) dst ->
            packOrDrop a itemSize dst
        )
        (container size)
        <<< toArray


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


pinMany :: forall f l a. IsPinningLayout l => Foldable f => f (a /\ Pos /\ Size) -> l a -> l a
pinMany source layout =
    foldr
        (\(a /\ pos /\ itemSize) -> pin a pos itemSize)
        layout
        source



-- packOrDrop

-- packOrDropMany


-- getByPos