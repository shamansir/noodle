module Web.Layout where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr, class Foldable)
import Data.Unfoldable (unfoldr, class Unfoldable)
import Data.FoldableWithIndex (class FoldableWithIndex)


import Data.Vec2 (Pos, Size, (<+>))
import Data.Vec2 (inside) as V2
import Data.Array ((:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Tuple (curry, uncurry)


import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Control.Alt ((<|>))


class {-FoldableWithIndex Pos l <=-} IsLayout l where
    --toFoldable :: forall f a. Foldable f => l a -> f (a /\ Pos /\ Size)
    --toUnfoldable :: forall f a. Unfoldable f => l a -> f (a /\ Pos /\ Size)
    --unfold :: forall a. l a -> Array (a /\ Pos /\ Size)
    -- TODO: make a function `a -> Pos -> Size -> k -> k`, then `fold' == fold (curry <<< curry f)`
    fold :: forall a k. ((a /\ Pos /\ Size) -> k -> k) -> k -> l a -> k
    find :: forall a. Eq a => a -> l a -> Maybe (Pos /\ Size) -- FIXME: make `find` use default implementation
    sample :: forall a. Pos -> l a -> Maybe (a /\ Pos /\ Size)  -- FIXME: make `sample` a default implementation
    --remove :: forall a. Eq a => a -> l a -> Maybe (l a)
    -- reflow :: forall a. Size -> l a -> Maybe (l a)

class IsLayout l <= IsSizedLayout l where
    size :: forall a. l a -> Size


class IsSizedLayout l <= IsContainerLayout l where
    container :: forall a. Size -> l a
    -- FIXME: `add :: a -> l a -> Maybe (l a)`

-- TODO: join `IsContainerLayout`` with `IsSizedLayout` back, because `container _ = empty` means "can have any size and may still work"

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


{-
fold :: forall a k. (a -> Pos -> Size -> k -> k) -> k -> l a -> k


fold' :: forall l k a. IsLayout l => ((a /\ Pos /\ Size) -> k -> k) -> k -> l a -> k
fold' f = fold (curry <<< curry f)
-}


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


-- FIMXE: make the default implementation of `Layout.find`
-- FIXME: test
findDefault :: forall l a. IsLayout l => Eq a => a -> l a -> Maybe (Pos /\ Size)
findDefault needle = fold findF Nothing
    where
        findF (item /\ itemPos /\ itemSize) = (<|>) $
            if needle == item then Just $ itemPos /\ itemSize else Nothing


-- FIMXE: make the default implementation of `Layout.sample`
-- FIXME: test
sampleDefault :: forall l a. IsLayout l => Pos -> l a -> Maybe (a /\ Pos /\ Size)
sampleDefault sPos = fold sampleF Nothing
    where
        sampleF (item /\ itemPos /\ itemSize) = (<|>) $
            if V2.inside sPos (itemPos /\ itemSize) then Just $ item /\ itemPos /\ itemSize else Nothing





-- packOrDrop

-- packOrDropMany


-- getByPos