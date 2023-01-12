module Layout.Flex.Axis
  ( Align(..)
  , Axis, Axis2, Axis3, Axis4
  , Padding(..)
  , PreEval(..)
  , items
  , align, alignStart, alignCenter, alignEnd, alignSpaceBetween, alignSpaceAround, alignSpaceEvenly, distributeWithGaps
  , padding
  --, class Container
  , fillSizes
  , find
  , find'
  , fit, fit2, fitToSquare
  , flatten2
  , justify
  , fold, fold2, fold2'
  , fold2N, fold2S
  , layout
  , make
  , make2
  , mapSize
  , posOf, posOf2
  --, lift, lift2
  , map2Size
--   , foldPrev
  , mapItems
  , vert, horz
  , sizeOf
  )
  where


import Prelude

import Web.App.Style.Order (Order)
import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Functor.Invariant (class Invariant)
import Data.Traversable (sequence)
import Data.Foldable (foldr, foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, Pos_, (<+>))
import Data.Vec2 as V2


import Layout.Flex.Rule (Rule(..))
-- TODO: `IsLayout` instance (AutoSizedLayout?)


data Padding
    = Padding (Number /\ Number)


data Align
    = Start
    | End
    | Center
    | SpaceAround
    | SpaceBetween
    | SpaceEvenly
    | Gap Number -- TODO: Gap Rule


-- TODO: constraints

-- TODO: append


{- data Direction
    = Horizontal
    | Vertical -}


-- Operator candidates: ⁅ ⁆ ≡ ⫴ ⊢ ⊣ ⊪ ⊩ ≬ ⟷ ⧦ ⟺ ∥ ⁞ ⁝ ‖ ᎒ ᎓ ੦ ᠁ … ‒ – — ― ⊲ ⊳ ⊽ ⎪ ⎜ ⎟ ⟺ ⟚ ⟛


data Axis s a = Axis (Array (s /\ Maybe a))

type Axis2 s a = Axis s (Axis s a)

-- type Square s a = Axis2 s a

type Axis3 s a = Axis2 s (Axis s a)

type Axis4 s a = Axis3 s (Axis s a)


-- TODO: IsLayout
-- fitting => solving (means)


-- items :: forall s a. Axis s a -> Array (s /\ a)
-- items (Axis xs) = Array.mapMaybe (\(s /\ maybeA) -> ((/\) s) <*> maybeA) xs


items :: forall s a. Axis s a -> Array (s /\ Maybe a)
items (Axis xs) = xs


items' :: forall s a. Axis s a -> Array (s /\ a)
items' = items >>> Array.mapMaybe sequence


instance functorAxis :: Functor (Axis s) where
    map :: forall a b. (a -> b) -> Axis s a -> Axis s b
    map f (Axis items) = Axis $ map (map f) <$> items


instance bifunctorAxis :: Bifunctor Axis where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> Axis s1 a -> Axis s2 b
    bimap f g (Axis items) = Axis $ bimap f (map g) <$> items


make :: forall s a. Array (s /\ Maybe a) -> Axis s a
make = Axis


make2 :: forall s a. Array (s /\ Maybe (Array (s /\ Maybe a))) -> Axis2 s a
make2 items = Axis $ map (map Axis) <$> items


vert :: forall s a. Array (s /\ Axis s a) -> Axis2 s a
vert = horz


horz :: forall s a. Array (s /\ a) -> Axis s a
horz = make <<< map (map Just)


align :: forall a. Number -> Align -> Axis Number a -> Axis Number a
align total how (Axis items) =
    Axis $ if sumTaken < total
            then doAlign how
            else items
    where
        sumTaken = Array.foldr (+) 0.0 (fst <$> items)
        count = Array.length items
        doAlign Start = items <> [ (total - sumTaken) /\ Nothing ]
        doAlign Center =
            let sideSpace = (total - sumTaken) / 2.0
            in
            [ sideSpace /\ Nothing ] <> items <> [ sideSpace /\ Nothing ]
        doAlign End =
            [ (total - sumTaken) /\ Nothing ] <> items
        doAlign SpaceBetween =
            let spaceBetween = (total - sumTaken) / (toNumber $ count - 1)
            in Array.intersperse (spaceBetween /\ Nothing) items
        doAlign SpaceAround =
            let oneSpace = (total - sumTaken) / toNumber count
                halfSpace = oneSpace / 2.0
            in [ halfSpace /\ Nothing ] <> Array.intersperse (oneSpace /\ Nothing) items <> [ halfSpace /\ Nothing ]
        doAlign SpaceEvenly =
            let evenSpace = (total - sumTaken) / toNumber (count + 1)
            in [ evenSpace /\ Nothing ] <> Array.intersperse (evenSpace /\ Nothing) items <> [ evenSpace /\ Nothing ]
        doAlign (Gap n) =
            Array.intersperse (n /\ Nothing) items


-- TODO :: align2


alignStart :: forall a. Number -> Axis Number a -> Axis Number a
alignStart n = align n Start


-- alignStart' :: forall a. Number -> Flex Rule a -> Flex Number (Cell a)
-- alignStart' n = fit n >>> alignStart n


alignCenter :: forall a. Number -> Axis Number a -> Axis Number a
alignCenter n = align n Center


-- alignCenter' :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
-- alignCenter' n = fit n >>> alignCenter n


alignEnd :: forall a. Number -> Axis Number a -> Axis Number a
alignEnd n = align n End


alignSpaceBetween :: forall a. Number -> Axis Number a -> Axis Number a
alignSpaceBetween n = align n SpaceBetween


alignSpaceAround :: forall a. Number -> Axis Number a -> Axis Number a
alignSpaceAround n = align n SpaceAround


alignSpaceEvenly :: forall a. Number -> Axis Number a -> Axis Number a
alignSpaceEvenly n = align n SpaceEvenly


distributeWithGaps :: forall a. Number -> Number -> Axis Number a -> Axis Number a
distributeWithGaps total gap = align total $ Gap gap


padding :: forall a. (Number /\ Number) -> Axis Number a -> Axis Number a
padding (start /\ end) (Axis items) =
    Axis $ [ start /\ Nothing ] <> items <> [ end /\ Nothing ]



justify :: forall a. Array a -> Axis Rule a
justify items = Axis $ ((/\) (Portion 1)) <$> Just <$> items

-- TODO: fitAll a.k.a. distribute a.k.a justify

-- TODO


data PreEval
    = Known Number
    | Portion_ Int


fit :: forall a. Number -> Axis Rule a -> Axis Number a -- TODO: Semiring n => Flex n a, Container f => f n a
fit amount (Axis items) =
    -- FIXME: take align and padding into consideration
    Axis $ Array.zip (justify_ (fst <$> items)) (snd <$> items)
    where
        justify_ :: Array Rule -> Array Number
        justify_ rules =
            fillPortionAmount <$> preEvaluated
            where

                preEvaluate (Portion n) = Portion_ n
                preEvaluate (Units n) = Known n
                preEvaluate (Percentage p) = Known $ amount * (toNumber p / 100.0)
                preEvaluate (Min min rule) =
                    case preEvaluate rule of
                        Known n -> Known $ if n >= min then n else min
                        Portion_ i -> Portion_ i -- FIXME
                preEvaluate (Max max rule) =
                    case preEvaluate rule of
                        Known n -> Known $ if n <= max then n else max
                        Portion_ i -> Portion_ i -- FIXME
                preEvaluate (MinMax (min /\ max) rule) =
                    case preEvaluate rule of
                        Known n -> Known $ if n <= min then min else if n >= max then max else n
                        Portion_ i -> Portion_ i -- FIXME

                --toKnownAmount (Cells c) = Just $ c * cellSize

                preEvaluated = preEvaluate <$> rules

                extractKnown (Known n) = Just n
                extractKnown (Portion_ _) = Nothing

                extractPortion (Portion_ n) = Just n
                extractPortion (Known _) = Nothing

                portionCount = Array.foldr (+) 0 $ Array.catMaybes $ map extractPortion preEvaluated
                knownAmount = Array.foldr (+) 0.0 $ Array.catMaybes $ map extractKnown preEvaluated

                fillPortionAmount (Known n) = n
                fillPortionAmount (Portion_ n) = ((amount - knownAmount) / toNumber portionCount) * toNumber n



fit2 :: forall a. Size -> Axis2 Rule a -> Axis2 Number a
fit2 size vflex =
    (fit $ V2.w size) <$> (fit (V2.h size) vflex)


fitToSquare :: forall a. Axis2 Rule a -> Axis2 Number a
fitToSquare = fit2 $ 1.0 <+> 1.0


-- TODO: fitWrap (cut oversize)


-- add width data to vertical boxes and height data to horizontal ones
fillSizes :: forall a. Axis2 Number a -> Axis2 Size a
fillSizes (Axis vitems) =
    Axis
        $ (\(h /\ maybeHAxis) ->
            case maybeHAxis of
                Just (Axis hitems) ->
                    ((foldr (+) 0.0 (fst <$> hitems)) <+> h)
                    /\
                    (Just $ lmap (\w' -> w' <+> h) $ Axis hitems)
                Nothing ->
                    (0.0 <+> h) /\ Nothing
        ) <$> vitems


mapItems :: forall s a s' a'. (s /\ Maybe a -> s' /\ Maybe a') -> Axis s a -> Axis s' a'
mapItems f (Axis items) = Axis $ map f items


{- lift :: forall s a. Axis s a -> Axis s (Cell a)
lift = map Taken


lift2 :: forall s a. Axis2 s a -> Axis2 s (Cell a)
lift2 = map $ map Taken -}


layout :: forall a. Size -> Axis2 Rule a -> Axis2 Size a
layout size = fit2 size >>> fillSizes


mapSize :: forall s1 s2 a. (s1 -> s2) -> Axis s1 a -> Axis s2 a
mapSize = lmap


map2Size :: forall s1 s2 a. (s1 -> s2) -> Axis2 s1 a -> Axis2 s2 a
map2Size f = lmap f <<< map (lmap f)


-- mapSize' :: forall s1 s2 a. (Direction -> s1 -> s2) -> Ordered s1 a -> Ordered s2 a
-- mapSize' f (Vert items) = Vert $ bimap (f Vertical) (lmap $ f Horizontal) <$> items


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


find :: forall n a. Semiring n => Ord n => Pos_ n -> Axis2 n a -> Maybe a
find pos ordered =
    snd <$> snd <$> find' pos ordered


find' :: forall n a. Semiring n => Ord n => Pos_ n -> Axis2 n a -> Maybe (Pos_ n /\ Size_ n /\ a)
find' pos =
    flatten2 >>> -- FIXME: use Unfoldable for faster search?
        foldl
            (\_ (pos' /\ size /\ maybeA) ->
                maybeA >>=
                    \a -> if V2.inside pos (pos' /\ size) then Just (pos' /\ size /\ a) else Nothing
            )
            Nothing


{- get :: forall s a. Int /\ Int -> Ordered s a -> Maybe (Size_ s /\ a)
get (ny /\ nx) (Vert vbox) =
    Array.index vbox ny
        >>= (\(h /\ (Horz hbox)) -> ((<+>) h) <$> Array.index hbox nx) -}


{-
toUnfoldable2 :: forall f s a. Unfoldable f => Flex2 s a -> f (Size_ s /\ a)
toUnfoldable2 =
    flatten2' >>> Array.toUnfoldable -- TODO: make unfoldable manually?
-}


{- sizeOf :: forall s a. Eq a => a -> Flex s a -> Maybe (Size_ s)
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
        vbox -}


posOf :: forall a. Eq a => a -> Axis Number a -> Maybe Pos
posOf _ _ = Nothing -- FIXME: implement


posOf2 :: forall a. Eq a => a -> Axis2 Number a -> Maybe Pos
posOf2 _ _ = Nothing -- FIXME: implement


sizeOf :: forall a. Axis2 Number a -> Size
sizeOf = const $ 0.0 <+> 0.0 -- FIXME: implement


{- fold2 :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Rule a -> b
fold2 f d = fit2 (1.0 <+> 1.0) >>> fold2' f d -}


-- foldPrev :: forall s a. Semigroup s => (s -> s -> a -> b -> b) -> b -> Axis2 nsa -> b
-- foldPrev


fold :: forall s a b. (s -> Maybe a -> b -> b) -> b -> Axis s a -> b
fold f def = foldl (flip $ uncurry f) def <<< items


fold' :: forall s a b. (s -> a -> b -> b) -> b -> Axis s a -> b
fold' f = fold $ \s maybeA b -> maybe b (\a -> f s a b) maybeA


-- fold' :: forall s a b. (s -> Maybe a -> b -> b) -> b -> Axis s a -> b
-- fold' f def (Axis items) = foldr (uncurry f) def items


fold2 :: forall s a b. ((s /\ Maybe s) -> Maybe a -> b -> b) -> b -> Axis2 s a -> b
fold2 f def =
    foldl
        (\b (s /\ maybeAxis) ->
            case maybeAxis of
                Just axis -> fold (\s' -> f (s /\ Just s')) b axis
                Nothing -> f (s /\ Nothing) Nothing b

        )
        def
        <<< items


fold2' :: forall s a b. ((s /\ s) -> a -> b -> b) -> b -> Axis2 s a -> b
fold2' f def =
    foldl
        (\b (s /\ axis) ->
            fold' (\s' -> f (s /\ s')) b axis
        )
        def
        <<< items'


{- foldPrev :: forall s a b. (Array s -> s -> a -> b -> b) -> b -> Axis s a -> b
foldPrev f =
    foldPrev' $ \prev s maybeA b -> case maybeA of
        Just a -> f prev s a b
        Nothing -> b


foldPrev' :: forall s a b. (Array s -> s -> Maybe a -> b -> b) -> b -> Axis s a -> b
foldPrev' f def (Axis items) =
    snd $ foldr
        (\(s /\ a) (prev /\ b) ->
            Array.snoc prev s /\ f prev s a b
        )
        ([] /\ def)
        items
-}


fold2N :: forall n a b. Semiring n => (Pos_ n -> Size_ n -> Maybe a -> b -> b) -> b -> Axis2 n a -> b
fold2N f def vaxis =
    snd $ foldl
        (\(y /\ b) (h /\ haxis) ->
            (y + h)
            /\
            maybe b
                (snd
                    <<< foldl
                        (\(x /\ b') (w /\ a) ->
                            (x + w)
                            /\
                            (f (x <+> y) (w <+> h) a b')
                        )
                        (zero /\ b)
                    <<< items
                )
                haxis
        )
        (zero /\ def)
        $ items vaxis


fold2S :: forall n a b. Semiring n => (Pos_ n -> Size_ n -> Maybe a -> b -> b) -> b -> Axis2 (Size_ n) a -> b
fold2S f def vaxis =
    snd $ foldl
        (\(y /\ b) (vsize /\ haxis) ->
            (y + V2.h vsize)
            /\
            maybe b
                (snd
                    <<< foldl
                        (\(x /\ b') (hsize /\ a) ->
                            (x + V2.w hsize)
                            /\
                            (f (x <+> y) hsize a b')
                        )
                        (zero /\ b)
                    <<< items
                )
                haxis
        )
        (zero /\ def)
        $ items vaxis


{- fold2''' :: forall s a b. (Size_ s -> a -> b -> b) -> b -> Flex2 s a -> b
fold2''' f def (Flex vitems) =
    foldr
        (\(h /\ Flex hitems) b ->
            foldr
                (\(w /\ a) b' ->
                    f (w <+> h) a b'
                )
                b
                hitems
        )
        def
        vitems -}


{- unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = [] -- fold (curry <<< ?wh) [] -}


-- unfold' :: forall a. Ordered Number a -> Array (Pos /\ Size /\ Array (Pos /\ Size /\ a))
-- unfold' _ = []


flatten2 :: forall n a. Semiring n => Axis2 n a -> Array (Pos_ n /\ Size_ n /\ Maybe a)
flatten2 = fold2N (\p s a arr -> (p /\ s /\ a) : arr) []


-- flatten2' :: forall s a. Flex2 s a -> Array (Size_ s /\ a)
-- flatten2' = fold2''' (curry (:)) []