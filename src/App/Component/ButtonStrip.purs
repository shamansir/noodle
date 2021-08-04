module App.Component.ButtonStrip where


import Prelude

import Data.Int (toNumber, floor)
import Data.Vec2 (Vec2, Size, Pos, (<+>))
import Data.Vec2 as V2
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Functor.Invariant (imap)

import Noodle.Node


data ButtonStrip a = ButtonStrip Size (Array (Pos /\ a)) -- Bin2 Number Node.Family


buttonSize :: Size
buttonSize = 60.0 <+> 20.0


buttonPadding :: Vec2
buttonPadding = 5.0 <+> 15.0


size :: forall a. ButtonStrip a -> Size
size (ButtonStrip size _) = size


make :: forall a. Number -> Set a -> ButtonStrip a
make maxWidth items =
    let
        itemsCount = Set.size items
        fullButtonWidth = (V2.w buttonSize + V2.x buttonPadding)
        fullButtonHeight = (V2.h buttonSize + V2.y buttonPadding)
        maxButtonsX = floor maxWidth
        fitsInWidthX = (maxButtonsX `div` floor fullButtonWidth) * floor fullButtonWidth
        buttonPos idx =
            let
                linearX = (toNumber idx * fullButtonWidth)
                modX = toNumber $ floor linearX `mod` fitsInWidthX
                divX = toNumber $ floor linearX `div` fitsInWidthX
            in (V2.x buttonPadding + modX) <+> (divX * fullButtonHeight)
        bottomButtonsY = (V2.y $ buttonPos $ itemsCount - 1) + fullButtonHeight
        addPos (idx /\ a) = buttonPos idx /\ a
    in
        ButtonStrip
            (maxWidth <+> bottomButtonsY)
            $ map addPos $ Array.mapWithIndex (/\) $ Set.toUnfoldable items


unfold :: forall a. ButtonStrip a -> Array (Pos /\ a)
unfold (ButtonStrip _ items) = items