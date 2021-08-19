module Data.Layout.Ordered where


import Prelude

import App.Style.Order (Order)


import Data.Maybe (Maybe(..))
import Data.Vec2 (Size)

import Data.Tuple.Nested ((/\), type (/\))


-- TODO: `IsLayout` instance (AutoSizedLayout?)


cellSize :: Number
cellSize = 45.0

data Rule
    = Auto
    | Fixed Number
    | Percentage Number
    | Cells Number


-- data HBox s a = Horz (Array (s /\ VBox s a))
-- data VBox s a = Vert (Array (s /\ a))


data HBox s a = Horz (Array (s /\ a))

data VBox s a = Vert (Array (s /\ HBox s a))


type Ordered s a = VBox s a


auto :: Rule
auto = Auto


fixed :: Number -> Rule
fixed = Fixed


percents :: Number -> Rule
percents = Percentage


cells :: Number -> Rule
cells = Cells


make :: forall s a. Array (s /\ Array (s /\ a)) -> Ordered s a
make items = Vert $ map Horz <$> items


fit :: forall a. Size -> Ordered Rule a -> Ordered Size a
fit _ _ = Vert [] -- TODO


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


sizeOf :: forall s a. Eq a => a -> Ordered s a -> Maybe s
sizeOf _ _ = Nothing


unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = []


flatten :: forall s a. Ordered s a -> Array (s /\ s /\ a)
flatten _ = []


-- toUnfoldable ::


fold :: forall s a b. (s -> s -> a -> b) -> b -> Ordered s a -> b
fold f def _ = def