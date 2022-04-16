module App.Layout.Flex
  ( Align(..)
  , Cell(..)
  , Flex(..)
  , Flex2
  , Flex3
  , Flex4
  , Padding(..)
  , PreEval(..)
  , Rule(..)
  , align, alignStart, alignCenter, alignEnd, alignSpaceBetween, alignSpaceAround, alignSpaceEvenly, distributeWithGaps
  , padding
  --, class Container
  , fillSizes
  , find
  , find'
  , fit, fit2, fitToSquare
  , flatten2
  , justify
  , fold2
  , fold2'
  , layout
  , make
  , make2
  , mapSize
  , posOf
  , lift, lift2, map2Size
  )
  where


import Prelude

import App.Style.Order (Order)
import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, (<+>))
import Data.Vec2 as V2


-- TODO: `IsLayout` instance (AutoSizedLayout?)


data Rule
    = Portion Int -- a.k.a Fill a.k.a Portion 1
    | Units Number -- a.k.a. Px or Units
    | Percentage Int
    -- | Cells Number
    | Min Number Rule
    | Max Number Rule
    | MinMax (Number /\ Number) Rule
    -- | TODO: Fn (a -> Rule a)


data Cell a
    = Taken a
    | Space


data Padding
    = NoPadding
    | Padding (Number /\ Number)


data Align
    = Start
    | End
    | Center
    | SpaceAround
    | SpaceBetween
    | SpaceEvenly
    | Gap Number -- TODO: Gap Rule


-- TODO: padding + spacing (VBox items have padding and Horz items have spacing)

-- TODO: constraints

-- add align + centering + distribute


-- TODO: append


-- data HBox s a = Horz (Array (s /\ VBox s a))
-- data VBox s a = Vert (Array (s /\ a))


{- data Direction
    = Horizontal
    | Vertical -}


-- Operator candidates: ⁅ ⁆ ≡ ⫴ ⊢ ⊣ ⊪ ⊩ ≬ ⟷ ⧦ ⟺ ∥ ⁞ ⁝ ‖ ᎒ ᎓ ੦ ᠁ … ‒ – — ― ⊲ ⊳ ⊽ ⎪ ⎜ ⎟ ⟺ ⟚ ⟛


{- data HBox s a = Horz Padding Align (Array (s /\ a))


data VBox s a = Vert Padding Align (Array (s /\ HBox s a)) -}


-- FIXME:

data Flex s a = Flex (Array (s /\ a))

type Flex2 s a = Flex s (Flex s a)

type Flex3 s a = Flex2 s (Flex s a)


type Flex4 s a = Flex3 s (Flex s a)



{- class Container (x :: Type -> Type -> Type) s a | x -> s, x -> a where
    items :: x s a -> Array (s /\ a)
    fit_ :: Number -> x Rule a -> x Number a
    -- fit' :: Number -> x Rule (Cell a) -> x Number (Сell a)
    align_ :: Number -> Align -> x Rule a -> x Number (Cell a)
    takes :: x Number a -> Number
    from :: Array (s /\ a) -> x s a -}

-- Rule as Container ?


-- both VBox and HBox implement `IsLayout`
-- plus, both may distribute or align inner items

-- fitting => solving (means)



{-
type FlexN a = Flex Number a


type FlexR a = Flex Rule a


type FlexS a = Flex Rule a
-}


instance functorFlex :: Functor (Flex s) where
    map :: forall a b. (a -> b) -> Flex s a -> Flex s b
    map f (Flex items) = Flex $ map f <$> items


instance bifunctorFlex :: Bifunctor Flex where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> Flex s1 a -> Flex s2 b
    bimap f g (Flex items) = Flex $ bimap f g <$> items


make :: forall s a. Array (s /\ a) -> Flex s a
make = Flex


make2 :: forall s a. Array (s /\ Array (s /\ a)) -> Flex2 s a
make2 items = Flex $ map Flex <$> items


align :: forall a. Number -> Align -> Flex Number a -> Flex Number (Cell a)
align total how (Flex items) =
    Flex $ if sumTaken < total
            then doAlign how
            else (map Taken <$> items)
    where
        sumTaken = Array.foldr (+) 0.0 (fst <$> items)
        count = Array.length items
        doAlign Start = (map Taken <$> items) <> [ (total - sumTaken) /\ Space ]
        doAlign Center =
            let sideSpace = (total - sumTaken) / 2.0
            in
            [ sideSpace /\ Space ] <> (map Taken <$> items) <> [ sideSpace /\ Space ]
        doAlign End =
            [ (total - sumTaken) /\ Space ] <> (map Taken <$> items)
        doAlign SpaceBetween =
            let spaceBetween = (total - sumTaken) / (toNumber $ count - 1)
            in Array.intersperse (spaceBetween /\ Space) (map Taken <$> items)
        doAlign SpaceAround =
            let oneSpace = (total - sumTaken) / toNumber count
                halfSpace = oneSpace / 2.0
            in [ halfSpace /\ Space ] <> Array.intersperse (oneSpace /\ Space) (map Taken <$> items) <> [ halfSpace /\ Space ]
        doAlign SpaceEvenly =
            let evenSpace = (total - sumTaken) / toNumber (count + 1)
            in [ evenSpace /\ Space ] <> Array.intersperse (evenSpace /\ Space) (map Taken <$> items) <> [ evenSpace /\ Space ]
        doAlign (Gap n) =
            Array.intersperse (n /\ Space) (map Taken <$> items)


alignStart :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignStart n = align n Start


-- alignStart' :: forall a. Number -> Flex Rule a -> Flex Number (Cell a)
-- alignStart' n = fit n >>> alignStart n


alignCenter :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignCenter n = align n Center


-- alignCenter' :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
-- alignCenter' n = fit n >>> alignCenter n


alignEnd :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignEnd n = align n End


alignSpaceBetween :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignSpaceBetween n = align n SpaceBetween


alignSpaceAround :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignSpaceAround n = align n SpaceAround


alignSpaceEvenly :: forall a. Number -> Flex Number a -> Flex Number (Cell a)
alignSpaceEvenly n = align n SpaceEvenly


distributeWithGaps :: forall a. Number -> Number -> Flex Number a -> Flex Number (Cell a)
distributeWithGaps total gap = align total $ Gap gap


padding :: forall a. (Number /\ Number) -> Flex Number a -> Flex Number (Cell a)
padding (start /\ end) (Flex items) =
    Flex $ [ start /\ Space ] <> (map Taken <$> items) <> [ end /\ Space ]



justify :: forall s a. Array a -> Flex Rule a
justify items = Flex $ ((/\) (Portion 1)) <$> items

-- TODO: fitAll a.k.a. distribute a.k.a justify

-- TODO


data PreEval
    = Known Number
    | Portion_ Int


fit :: forall a. Number -> Flex Rule a -> Flex Number a -- TODO: Semiring n => Flex n a, Container f => f n a
fit amount (Flex items) =
    -- FIXME: take align and padding into consideration
    Flex $ Array.reverse $ Array.zip (justify_ (fst <$> items)) (snd <$> items)
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



fit2 :: forall a. Size -> Flex2 Rule a -> Flex2 Number a
fit2 size vflex =
    (fit $ V2.w size) <$> (fit (V2.h size) vflex)


fitToSquare :: forall a. Flex2 Rule a -> Flex2 Number a
fitToSquare = fit2 $ 1.0 <+> 1.0


-- TODO: fitWrap (cut oversize)


-- add width data to vertical boxes and height data to horizontal ones
fillSizes :: forall a. Flex2 Number a -> Flex2 Size a
fillSizes (Flex vitems) =
    Flex
        $ (\(h /\ Flex hitems) ->
            ((foldr (+) 0.0 (fst <$> hitems)) <+> h)
            /\
            (lmap (\w' -> w' <+> h) $ Flex hitems)
        ) <$> vitems


lift :: forall s a. Flex s a -> Flex s (Cell a)
lift = map Taken


lift2 :: forall s a. Flex2 s a -> Flex2 s (Cell a)
lift2 = map $ map Taken


layout :: forall a. Size -> Flex2 Rule a -> Flex2 Size a
layout size = fit2 size >>> fillSizes


mapSize :: forall s1 s2 a. (s1 -> s2) -> Flex s1 a -> Flex s2 a
mapSize = lmap


map2Size :: forall s1 s2 a. (s1 -> s2) -> Flex2 s1 a -> Flex2 s2 a
map2Size f = lmap f <<< map (lmap f)


-- mapSize' :: forall s1 s2 a. (Direction -> s1 -> s2) -> Ordered s1 a -> Ordered s2 a
-- mapSize' f (Vert items) = Vert $ bimap (f Vertical) (lmap $ f Horizontal) <$> items


{- tryHorz :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryHorz _ _ ordered = ordered


tryVert :: forall a. Rule -> a -> Ordered Rule a -> Maybe (Ordered Rule a)
tryVert _ _ ordered = ordered -}


find :: forall a. Pos -> Flex2 Number a -> Maybe a
find pos ordered =
    snd <$> snd <$> find' pos ordered


find' :: forall a. Pos -> Flex2 Number a -> Maybe (Pos /\ Size /\ a)
find' pos =
    flatten2 >>> -- FIXME: use Unfoldable for faster search?
        foldr
            (\(pos' /\ size /\ a) _ ->
                if V2.inside pos (pos' /\ size) then Just (pos' /\ size /\ a) else Nothing
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


posOf :: forall a. Eq a => a -> Flex Number a -> Maybe Pos
posOf _ _ = Nothing


{- fold2 :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Rule a -> b
fold2 f d = fit2 (1.0 <+> 1.0) >>> fold2' f d -}


fold2 :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Number a -> b -- TODO: Number -> Semiring, where possible
fold2 f def (Flex vitems) =
    snd $ foldr
        (\(h /\ Flex hitems) (y /\ b) ->
            (y + h)
            /\
            (snd $ foldr
                (\(w /\ a) (x /\ b') ->
                    (x + w)
                    /\
                    (f (x <+> y) (w <+> h) a b')
                )
                (0.0 /\ b)
                hitems
            )
        )
        (0.0 /\ def)
        vitems


fold2' :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Size a -> b
fold2' f def (Flex vitems) =
    snd $ foldr
        (\(vsize /\ Flex hitems) (y /\ b) ->
            (y + V2.h vsize)
            /\
            (snd $ foldr
                (\(hsize /\ a) (x /\ b') ->
                    (x + V2.w hsize)
                    /\
                    (f (x <+> y) hsize a b')
                )
                (0.0 /\ b)
                hitems
            )
        )
        (0.0 /\ def)
        vitems


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


flatten2 :: forall a. Flex2 Number a -> Array (Pos /\ Size /\ a)
flatten2 = fold2 (\p s a arr -> (p /\ s /\ a) : arr) []


-- flatten2' :: forall s a. Flex2 s a -> Array (Size_ s /\ a)
-- flatten2' = fold2''' (curry (:)) []