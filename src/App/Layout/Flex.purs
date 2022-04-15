module App.Layout.Flex
  ( Align(..)
  , Cell(..)
  , Flex(..)
  , Flex2
  , Flex3
  , Padding(..)
  , PreEval(..)
  , Rule(..)
  , alignPlain
  --, class Container
  , fillSizes
  , find
  , find'
  , fit, fit2
  , flatten2
  , fold2
  , fold2'
  , foldInSquare
  , layout
  , make
  , make2
  , mapSize, map2Size
  , posOf
  --, toUnfoldable2
  )
  where


import Prelude

import App.Style.Order (Order)
import CSS (b)
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
    = Portion Int
    | Units Number -- a.k.a. Px or Units
    | Percentage Int
    -- | Cells Number
    | Min Number Rule
    | Max Number Rule
    | MinMax (Number /\ Number) Rule
    -- | TODO: Fn (a -> Rule a)


data Cell s a
    = Taken (s /\ a)
    | Space s


data Padding
    = Padding (Number /\ Number)


data Align
    = Fill -- a.k.a. Justify
    | Start
    | End
    | Center
    | SpaceAround
    | SpaceBetween
    | SpaceEvenly
    | Gap Number -- TODO: Gap Rule


-- TODO: constraints

-- Operator candidates: ⁅ ⁆ ≡ ⫴ ⊢ ⊣ ⊪ ⊩ ≬ ⟷ ⧦ ⟺ ∥ ⁞ ⁝ ‖ ᎒ ᎓ ੦ ᠁ … ‒ – — ― ⊲ ⊳ ⊽ ⎪ ⎜ ⎟ ⟺ ⟚ ⟛


data Flex s a = Flex (Array (Cell s a))

type Flex2 s a = Flex s (Flex s a)

type Flex3 s a = Flex2 s (Flex s a)


derive instance functorCell :: Functor (Cell s)


instance bifunctorCell :: Bifunctor Cell where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> Cell s1 a -> Cell s2 b
    bimap f g (Taken (s /\ a)) = Taken $ f s /\ g a
    bimap f g (Space s) = Space $ f s


instance functorFlex :: Functor (Flex s) where
    map :: forall a b. (a -> b) -> Flex s a -> Flex s b
    map f (Flex items) = Flex $ map f <$> items


instance bifunctorFlex :: Bifunctor Flex where
    bimap :: forall s1 s2 a b. (s1 -> s2) -> (a -> b) -> Flex s1 a -> Flex s2 b
    bimap f g (Flex items) = Flex $ bimap f g <$> items


noPadding :: Padding
noPadding = Padding $ 0.0 /\ 0.0


make :: forall s a. Array (s /\ a) -> Flex s a
make = Flex <<< map Taken


make2 :: forall s a. Array (s /\ Array (s /\ a)) -> Flex2 s a
--make2 items = Flex noPadding $ map (Flex noPadding) <$> items
make2 = make <<< map (map make)


alignPlain :: forall a. Number -> Align -> Array (Number /\ a) -> Array (Number /\ Cell a)
alignPlain total how items =
    if sumTaken < total
        then doAlign how
        else map Taken <$> items
    where
        sumTaken = Array.foldr (+) 0.0 (fst <$> items)
        count = Array.length items
        doAlign Fill = doAlign Start -- FIXME
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


-- TODO: fitAll a.k.a. distribute a.k.a justify

-- TODO


data PreEval
    = Known Number
    | Portion_ Int


-- fit :: forall n a. Semiring n => n -> Flex Rule a -> Flex n a
-- fit :: forall c a. Container c => Number -> c Rule a -> c n a
fit :: forall a. Number -> Flex Rule a -> Flex Number a
fit amount (Flex padding items) =
    -- FIXME: take padding into consideration
    Flex padding $ Array.reverse $ distribute amount items
    where
        distribute :: Number -> Array (Cell (Rule /\ a)) -> Array (Cell (Number /\ a))
        distribute amount rules =
            map fillEvaluated $ Array.zip preEvaluated rules
            -- fillPortionAmount <$> preEvaluated
            where

                preEvaluateRule (Portion n) = Portion_ n
                preEvaluateRule (Units n) = Known n
                preEvaluateRule (Percentage p) = Known $ (amount - amountOfSpace) * (toNumber p / 100.0)
                preEvaluateRule (Min min rule) =
                    case preEvaluateRule rule of
                        Known n -> Known $ if n >= min then n else min
                        Portion_ i -> Portion_ i -- FIXME
                preEvaluateRule (Max max rule) =
                    case preEvaluateRule rule of
                        Known n -> Known $ if n <= max then n else max
                        Portion_ i -> Portion_ i -- FIXME
                preEvaluateRule (MinMax (min /\ max) rule) =
                    case preEvaluateRule rule of
                        Known n -> Known $ if n <= min then min else if n >= max then max else n
                        Portion_ i -> Portion_ i -- FIXME

                extractKnown (Known n) = Just n
                extractKnown (Portion_ _) = Nothing

                extractPortion (Portion_ n) = Just n
                extractPortion (Known _) = Nothing

                extractSpace (Space n) = Just n
                extractSpace (Taken _) = Nothing

                preEvaluateCell (Taken (rule /\ a)) = preEvaluateRule rule
                preEvaluateCell (Space n) = Known n

                preEvaluated = preEvaluateCell <$> rules

                portionCount = Array.foldr (+) 0 $ Array.catMaybes $ map extractPortion preEvaluated
                knownAmount = Array.foldr (+) 0.0 $ Array.catMaybes $ map extractKnown preEvaluated
                amountOfSpace = Array.foldr (+) 0.0 $ Array.catMaybes $ map extractSpace rules

                fillEvaluated :: PreEval /\ Cell (Rule /\ a) -> Cell (Number /\ a)
                fillEvaluated (_ /\ Space space) = Space space
                fillEvaluated (Known n /\ Taken (_ /\ a)) = Taken $ n /\ a
                fillEvaluated (Portion_ p /\ Taken (_ /\ a)) = Taken $ evaluatePortion p /\ a

                evaluatePortion p = ((amount - knownAmount - amountOfSpace) / toNumber portionCount) * toNumber p



fit2 :: forall a. Size -> Flex2 Rule a -> Flex2 Number a
fit2 size vflex =
    (fit $ V2.w size) <$> (fit (V2.h size) vflex)


fitToSquare :: Flex2 Rule a -> Flex2 Number a
fitToSquare = fit2 $ 1.0 <+> 1.0


-- TODO: fitWrap (cut oversize)


-- add width data to vertical boxes and height data to horizontal ones
fillSizes :: forall a. Flex2 Number a -> Flex2 Size a
fillSizes (Flex vpadding vitems) =
    Flex vpadding
        $ (\(h /\ (Flex hpadding hitems)) ->
            ((foldr (+) 0.0 (fst <$> hitems)) <+> h)
            /\
            (lmap (\w' -> w' <+> h) $ Flex hpadding hitems)
        ) <$> vitems


layout :: forall a. Size -> Flex2 Rule a -> Flex2 Size a
layout size = fit2 size >>> fillSizes


mapSize :: forall s1 s2 a. (s1 -> s2) -> Flex s1 a -> Flex s2 a
mapSize = lmap


map2Size :: forall s1 s2 a. (s1 -> s2) -> Flex2 s1 a -> Flex2 s2 a
map2Size f = lmap f <<< map (lmap f)


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


posOf :: forall a. Eq a => a -> Flex Number a -> Maybe Pos
posOf _ _ = Nothing


-- align :: Align -> Flex s a -> Flex s (Cell a)


-- padding :: Padding -> Flex s a -> Flex s (Cell a)


{- fold_ :: forall n a b. Semiring n => (n -> n -> Maybe a -> b -> b) -> b -> Flex2 n a -> b
fold_ f def (Flex vitems) =
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
                (zero /\ b)
                hitems
            )
        )
        (zero /\ def)
        vitems
    where
        foldv (Space n) ()
        foldh -}


foldInSquare :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Rule a -> b
foldInSquare f d = fit2 (1.0 <+> 1.0) >>> fold2' f d


fold2 :: forall a b. (Pos -> Size -> a -> b -> b) -> b -> Flex2 Number a -> b -- TODO: Number -> Semiring, where possible, Size_ n, Pos_ n
fold2 f def (Flex vpadding vitems) =
    snd $ foldr
        (\(h /\ Flex hpadding hitems) (y /\ b) ->
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
fold2' f def (Flex vpadding vitems) =
    snd $ foldr
        (\(vsize /\ Flex hpadding hitems) (y /\ b) ->
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


fold2WithSpace :: forall a b. (Pos -> Size -> Maybe a -> b -> b) -> b -> Flex2 Number a -> b
fold2WithSpace f def (Flex vitems) =
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
    where
        foldv (Space h) (y /\ b) =
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
        foldv (Taken h /\ Flex hitems) (y /\ b) =
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


{- fold2''' :: forall s a b. (Size_ s -> a -> b -> b) -> b -> Flex2 s a -> b
fold2''' f def (Flex vpadding vitems) =
    foldr
        (\(h /\ Flex hpadding hitems) b ->
            foldr
                (\(w /\ a) b' ->
                    f (w <+> h) a b'
                )
                b
                $ onlyValues hitems
        )
        def
        $ onlyValues vitems -}


{- unfold :: forall s a. Ordered s a -> Array (s /\ Array (s /\ a))
unfold _ = [] -- fold (curry <<< ?wh) [] -}


-- unfold' :: forall a. Ordered Number a -> Array (Pos /\ Size /\ Array (Pos /\ Size /\ a))
-- unfold' _ = []


flatten2 :: forall a. Flex2 Number a -> Array (Pos /\ Size /\ a)
flatten2 = fold2' (\p s a arr -> (p /\ s /\ a) : arr) []


{- flatten2' :: forall s a. Flex2 s a -> Array (Size_ s /\ a)
flatten2' = fold2''' (curry (:)) [] -}