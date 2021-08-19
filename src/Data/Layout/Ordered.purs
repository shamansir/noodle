module Data.Layout.Ordered where


import Prelude

import App.Style.Order (Order)


import Data.Maybe (Maybe(..))
import Data.Vec2 (Size)
import Data.Int (toNumber)
import Data.Array as Array

import Data.Tuple.Nested ((/\), type (/\))


-- TODO: `IsLayout` instance (AutoSizedLayout?)


cellSize :: Number
cellSize = 45.0

data Rule
    = Auto
    | Fixed Number
    | Percentage Int
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


percents :: Int -> Rule
percents = Percentage


cells :: Number -> Rule
cells = Cells


make :: forall s a. Array (s /\ Array (s /\ a)) -> Ordered s a
make items = Vert $ map Horz <$> items


fit :: forall a. Size -> Ordered Rule a -> Ordered Size a
fit _ _ = Vert [] -- TODO


fit' :: Number -> Array Rule -> Array Number
fit' size rules =
    fillAutoSize <$> maybeActualSizes
    where
        isAuto Auto = true
        isAuto _ = false
        toKnownSize Auto = Nothing
        toKnownSize (Fixed n) = Just n
        toKnownSize (Percentage p) = Just $ size * (toNumber p / 100.0)
        toKnownSize (Cells c) = Just $ c * cellSize
        autoCount = Array.length $ Array.filter isAuto $ rules
        maybeActualSizes =  toKnownSize <$> rules
        knownSize = Array.foldr (+) 0.0 $ Array.catMaybes maybeActualSizes
        fillAutoSize (Just n) = n
        fillAutoSize Nothing = (size - knownSize) / toNumber autoCount


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


sizeOf :: forall s a. Eq a => a -> Ordered s a -> Maybe s
sizeOf _ _ = Nothing


fold :: forall s a b. (s -> s -> a -> b) -> b -> Ordered s a -> b
fold f def _ = def


unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = []


flatten :: forall s a. Ordered s a -> Array (s /\ s /\ a)
flatten _ = []


-- toUnfoldable ::
