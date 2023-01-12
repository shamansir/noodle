module Layout.Strip
  where


import Prelude

import Unsafe.Coerce (unsafeCoerce)

import Data.Maybe (Maybe(..))
import Data.Int (toNumber, floor)
import Data.Vec2 (Vec2, Size, Pos, (<+>))
import Data.Vec2 as V2
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array

import Control.Alt ((<|>))

import Web.Layout (class IsLayout, class IsSizedLayout, findDefault, sampleDefault) --, findDefault, sampleDefault)


data Strip a = Strip (Size /\ Size) Size (Array (Pos /\ a)) -- Bin2 Number Node.Family
-- FIXME: we store size of the item just to implement `IsLayout`, get rid of it and caclulate it during folding


-- TODO: implement `IsAutoLayout`

instance stripIsLayout :: IsLayout Strip where
    fold = fold
    find = findDefault
    sample = sample -- sampleDefault


instance stripIsSizedLayout :: IsSizedLayout Strip where
    size = size


fold :: forall a k. ((a /\ Pos /\ Size) -> k -> k) -> k -> Strip a -> k
fold f b (Strip (itemSize /\ _) _ items) =
    Array.foldr foldF b items
    where
        foldF (pos /\ item) =
            f (item /\ pos /\ itemSize)


-- FIXME: test
-- sampleDefault :: forall l a. IsLayout l => Pos -> l a -> Maybe (a /\ Pos /\ Size)
sample :: forall a. Pos -> Strip a -> Maybe (a /\ Pos /\ Size)
-- FIMXE: !! the same as Layout.sampleDefault
sample sPos =
    fold sampleF Nothing
    where
        sampleF (item /\ itemPos /\ itemSize) = (<|>) $
            if V2.inside sPos (itemPos /\ itemSize) then Just $ item /\ itemPos /\ itemSize else Nothing


size :: forall a. Strip a -> Size
size (Strip _ size _) = size


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
            (itemSize /\ itemPadding)
            (maxWidth <+> bottomItemsY)
            $ map addPos $ Array.mapWithIndex (/\) $ Set.toUnfoldable items


-- FIXME: toFoldable
unfold :: forall a. Strip a -> Array (Pos /\ a)
unfold (Strip _ _ items) = items


reflow :: forall a. Ord a => Size /\ Size -> Number -> Strip a -> Strip a
reflow sizes newWidth =
    make sizes newWidth <<< Set.fromFoldable <<< (<$>) snd <<< unfold
