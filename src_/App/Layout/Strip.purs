module App.Layout.Strip where


import Prelude

import Data.Int (toNumber, floor)
import Data.Vec2 (Vec2, Size, Pos, (<+>))
import Data.Vec2 as V2
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Functor.Invariant (imap)

import Data.Vec2 (Pos, Size)
import App.Layout (class IsLayout, class IsAutoLayout)


data Strip a = Strip Size (Array (Pos /\ a)) -- Bin2 Number Node.Family


-- TODO: implement `IsLayout` & `IsAutoLayout`


size :: forall a. Strip a -> Size
size (Strip size _) = size


make :: forall a. Size /\ Size -> Number -> Set a -> Strip a
make (itemSize /\ itemPadding) maxWidth items =
    let
        itemsCount = Set.size items
        fullItemWidth = (V2.w itemSize + V2.x itemPadding)
        fullItemHeight = (V2.h itemSize + V2.y itemPadding)
        maxItemsX = floor maxWidth
        fitsInWidthX = (maxItemsX `div` floor fullItemWidth) * floor fullItemWidth
        buttonPos idx =
            let
                linearX = (toNumber idx * fullItemWidth)
                modX = toNumber $ floor linearX `mod` fitsInWidthX
                divX = toNumber $ floor linearX `div` fitsInWidthX
            in (V2.x itemPadding + modX) <+> (divX * fullItemHeight)
        bottomItemsY = (V2.y $ buttonPos $ itemsCount - 1) + fullItemHeight
        addPos (idx /\ a) = buttonPos idx /\ a
    in
        Strip
            (maxWidth <+> bottomItemsY)
            $ map addPos $ Array.mapWithIndex (/\) $ Set.toUnfoldable items


-- FIXME: toFoldable
unfold :: forall a. Strip a -> Array (Pos /\ a)
unfold (Strip _ items) = items


reflow :: forall a. Ord a => Size /\ Size -> Number -> Strip a -> Strip a
reflow sizes newWidth =
    make sizes newWidth <<< Set.fromFoldable <<< (<$>) snd <<< unfold
