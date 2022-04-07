module App.Layout.Flex where


import Prelude

import App.Style.Order (Order)


import Data.Maybe (Maybe(..))
import Data.Vec2 (Size, Size_, Pos, (<+>))
import Data.Vec2 as V2
import Data.Int (toNumber)
import Data.Array ((:))
import Data.Array as Array
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Data.Foldable (foldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Unfoldable (class Unfoldable, unfoldr)


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


{- data Direction
    = Horizontal
    | Vertical -}


-- Operator candidates: ⁅ ⁆ ≡ ⫴ ⊢ ⊣ ⊪ ⊩ ≬ ⟷ ⧦ ⟺ ∥ ⁞ ⁝ ‖ ᎒ ᎓ ੦ ᠁ … ‒ – — ― ⊲ ⊳ ⊽ ⎪ ⎜ ⎟ ⟺ ⟚ ⟛


data HBox s a = Horz (Array (s /\ a))


data VBox s a = Vert (Array (s /\ HBox s a))


type Flex s a = VBox s a


type FlexN a = Flex Number a


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


make :: forall s a. Array (s /\ Array (s /\ a)) -> Flex s a
make items = Vert $ map Horz <$> items


fit :: forall a. Size -> Flex Rule a -> Flex Number a
fit size (Vert vbox) =
    Vert result
    where

        result :: Array (Number /\ HBox Number a)
        result =
            map
                (\(Horz hbox) -> Horz $ fitPlain' (V2.w size) hbox)
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


fillSizes :: forall a. Flex Number a -> Flex Size a
fillSizes (Vert vbox) =
    Vert
        $ (\(h /\ (Horz hbox)) ->
            ((foldr (+) 0.0 (fst <$> hbox)) <+> h)
            /\
            (lmap (\w' -> w' <+> h) $ Horz hbox)
        ) <$> vbox


mapSize :: forall s1 s2 a. (s1 -> s2) -> Flex s1 a -> Flex s2 a
mapSize = lmap


-- mapSize' :: forall s1 s2 a. (Direction -> s1 -> s2) -> Ordered s1 a -> Ordered s2 a
-- mapSize' f (Vert items) = Vert $ bimap (f Vertical) (lmap $ f Horizontal) <$> items


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


find :: forall a. Pos -> Flex Number a -> Maybe a
find pos ordered =
    snd <$> snd <$> find' pos ordered


find' :: forall a. Pos -> Flex Number a -> Maybe (Pos /\ Size /\ a)
find' pos =
    flatten' >>> -- FIXME: use Unfoldable for faster search?
        foldr
            (\(pos' /\ size /\ a) _ ->
                if V2.inside pos (pos' /\ size) then Just (pos' /\ size /\ a) else Nothing
            )
            Nothing


{- get :: forall s a. Int /\ Int -> Ordered s a -> Maybe (Size_ s /\ a)
get (ny /\ nx) (Vert vbox) =
    Array.index vbox ny
        >>= (\(h /\ (Horz hbox)) -> ((<+>) h) <$> Array.index hbox nx) -}


toUnfoldable :: forall f s a. Unfoldable f => Flex s a -> f (Size_ s /\ a)
toUnfoldable =
    flatten >>> Array.toUnfoldable -- TODO: make unfoldable manually?


sizeOf :: forall s a. Eq a => a -> Flex s a -> Maybe (Size_ s)
sizeOf a (Vert vbox) =
    Array.findMap
        (\(h /\ (Horz hbox)) ->
            Array.findMap
                (\(w /\ item) ->
                    if item == a then Just (w <+> h)
                    else Nothing
                )
                hbox
        )
        vbox


posOf :: forall a. Eq a => a -> Flex Number a -> Maybe Pos
posOf _ _ = Nothing


fold :: forall s a b. (Size_ s -> a -> b -> b) -> b -> Flex s a -> b
fold f def (Vert vbox) =
    foldr
        (\(h /\ (Horz hbox)) b ->
            foldr
                (\(w /\ a) b' ->
                    f (w <+> h) a b'
                )
                b
                hbox
        )
        def
        vbox


foldWithPos :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex Number a -> b
foldWithPos f def (Vert vbox) =
    snd $ foldr
        (\(h /\ (Horz hbox)) (y /\ b) ->
            (y + h)
            /\
            (snd $ foldr
                (\(w /\ a) (x /\ b') ->
                    (x + w)
                    /\
                    (f (x <+> y) (w <+> h) a b')
                )
                (0.0 /\ b)
                hbox
            )
        )
        (0.0 /\ def)
        vbox


{- unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = [] -- fold (curry <<< ?wh) [] -}


-- unfold' :: forall a. Ordered Number a -> Array (Pos /\ Size /\ Array (Pos /\ Size /\ a))
-- unfold' _ = []


flatten :: forall s a. Flex s a -> Array (Size_ s /\ a)
flatten = fold (curry (:)) []


flatten' :: forall a. Flex Number a -> Array (Pos /\ Size /\ a)
flatten' = foldWithPos (\p s a arr -> (p /\ s /\ a) : arr) []
