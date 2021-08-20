module Data.Layout.Ordered where


import Prelude

import App.Style.Order (Order)


import Data.Maybe (Maybe(..))
import Data.Vec2 (Size, Pos, (<+>))
import Data.Vec2 as V2
import Data.Int (toNumber)
import Data.Array as Array
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))

import Data.Bifunctor (class Bifunctor, bimap, lmap)


-- TODO: `IsLayout` instance (AutoSizedLayout?)
-- TODO: Rename to `Flex`?


cellSize :: Number
cellSize = 45.0

data Rule
    = Auto
    | Fixed Number
    | Percentage Int
    | Cells Number


-- data HBox s a = Horz (Array (s /\ VBox s a))
-- data VBox s a = Vert (Array (s /\ a))


data Direction
    = Horizontal
    | Vertical


data HBox s a = Horz (Array (s /\ a))

data VBox s a = Vert (Array (s /\ HBox s a))


type Ordered s a = VBox s a


instance functorHBox :: Functor (HBox s) where
    map :: forall a b. (a -> b) -> HBox s a -> HBox s b
    map f (Horz items) = Horz $ map f <$> items


instance functorVBox :: Functor (VBox s) where
    map :: forall a b. (a -> b) -> VBox s a -> VBox s b
    map f (Vert items) = Vert $ map (map f) <$> items


instance bifunctorHBox :: Bifunctor HBox where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> HBox s1 a -> HBox s2 b
    bimap f g (Horz items) = Horz $ bimap f g <$> items


instance bifunctorVBox :: Bifunctor VBox where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> VBox s1 a -> VBox s2 b
    bimap f g (Vert items) = Vert $ bimap f (bimap f g) <$> items


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
fit size (Vert vbox) =
    Vert result
    where

        result :: Array (Size /\ HBox Size a)
        result =
            (\(h /\ (Horz hbox)) ->
                (V2.w size <+> h)
                /\
                (Horz $ (lmap (\w -> w <+> h)) <$> fitPlain' (V2.w size) hbox)
            )
            <$> verticalFit

        verticalFit :: Array (Number /\ HBox Rule a)
        verticalFit = fitPlain' (V2.h size) vbox


        fitPlain' :: forall x. Number -> Array (Rule /\ x) -> Array (Number /\ x)
        fitPlain' amount items = Array.zip (fitPlain amount (fst <$> items)) (snd <$> items)


        fitPlain :: Number -> Array Rule -> Array Number
        fitPlain amount rules =
            fillAutoAmount <$> maybeActualAmounts
            where

                isAuto Auto = true
                isAuto _ = false

                toKnownAmount Auto = Nothing
                toKnownAmount (Fixed n) = Just n
                toKnownAmount (Percentage p) = Just $ amount * (toNumber p / 100.0)
                toKnownAmount (Cells c) = Just $ c * cellSize

                autoCount = Array.length $ Array.filter isAuto $ rules

                maybeActualAmounts =  toKnownAmount <$> rules

                knownAmount = Array.foldr (+) 0.0 $ Array.catMaybes maybeActualAmounts

                fillAutoAmount (Just n) = n
                fillAutoAmount Nothing = (amount - knownAmount) / toNumber autoCount


mapSize :: forall s1 s2 a. (s1 -> s2) -> Ordered s1 a -> Ordered s2 a
mapSize = lmap


mapSize' :: forall s1 s2 a. (Direction -> s1 -> s2) -> Ordered s1 a -> Ordered s2 a
mapSize' f (Vert items) = Vert $ bimap (f Vertical) (lmap $ f Horizontal) <$> items


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


find :: forall a. Pos -> Ordered Size a -> Maybe a
find _ _ = Nothing


sizeOf :: forall s a. Eq a => a -> Ordered s a -> Maybe s
sizeOf _ _ = Nothing


fold :: forall s a b. (s -> s -> a -> b) -> b -> Ordered s a -> b
fold f def _ = def


unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = []


unfold' :: forall a. Ordered Size a -> Array (Pos /\ Size /\ Array (Pos /\ Size /\ a))
unfold' _ = []


flatten :: forall s a. Ordered s a -> Array (s /\ s /\ a)
flatten _ = []


flatten' :: forall a. Ordered Size a -> Array (Pos /\ Size /\ a)
flatten' _ = []


-- toUnfoldable ::
