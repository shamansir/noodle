module App.Component.ButtonStrip where


import Prelude

import Data.Vec2 (Vec2, Size, (<+>))
import Data.Tuple.Nested ((/\))
import Data.Set (Set)

import App.Layout.Strip (Strip)
import App.Layout.Strip as Strip


buttonSize :: Size
buttonSize = 60.0 <+> 20.0


buttonPadding :: Vec2
buttonPadding = 5.0 <+> 15.0


make :: forall a. Number -> Set a -> Strip a
make =
    Strip.make (buttonSize /\ buttonPadding)


reflow :: forall a. Ord a => Number -> Strip a -> Strip a
reflow =
    Strip.reflow (buttonSize /\ buttonPadding)
