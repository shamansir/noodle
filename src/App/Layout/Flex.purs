module App.Layout.Flex
  ( Flex
  , fit
  , flex, flex1
  , fold, foldN
  , nest, nest1
  )
  where


import Prelude

import Data.Array ((:))
-- import Data.Array.Ex ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, Pos_, (<+>))
import Data.Vec2 as V2

import App.Layout.Flex.Axis as Axis
import App.Layout.Flex.Axis (Axis2)
import App.Layout.Flex.Rule (Rule(..))


-- TODO: `IsLayout` instance (AutoSizedLayout?)


data Flex s a
    = Level (Axis2 s a)
    | Deeper (Axis2 s (Flex s a))


-- TODO: Functor, etc.


flex :: forall s a. Array (s /\ Array (s /\ a)) -> Flex s a
flex = Level <<< Axis.make2


flex1 :: forall s a. s -> Array (s /\ a) -> Flex s a
flex1 s arr = flex $ Array.singleton (s /\ arr)


nest :: forall s a. Array (s /\ Array (s /\ Flex s a)) -> Flex s a
nest = Deeper <<< Axis.make2


nest1 :: forall s a. s -> Array (s /\ Flex s a) -> Flex s a
nest1 s arr = nest $ Array.singleton (s /\ arr)


{- fold :: forall s a b. (Array s -> Array s -> s -> a -> b -> b) -> b -> Flex s a -> b
fold = foldAt []
    where
        foldAt path f def (Level axis) =
            Axis.foldPrev (f path) def axis
        foldAt path f def (Deeper faxis) =
            foldr
                (\(s /\ flex_) b ->
                    foldAt (Array.cons s path) f b flex_
                )
                def
                $ Array.reverse $ Axis.items faxis -- FIXME: why reverse? -}


fit :: forall a. Size -> Flex Rule a -> Flex Size a
fit size = fitAt size
    where
        fitAt pSize =
            case _ of
                Level axis2 -> Level $ Axis.layout pSize axis2
                Deeper faxis2 ->
                    Deeper
                        $ map (Axis.mapItems (\(cSize /\ axis2) -> cSize /\ fitAt cSize axis2))
                        -- $ map (Axis.mapItems (\(cSize /\ axis2) -> (pSize - cSize) /\ fitAt cSize axis2))
                        $ Axis.layout pSize faxis2


fillSizes :: forall a. Flex Number a -> Flex Size a
fillSizes =
    case _ of
        Level axis2 -> Level $ Axis.fillSizes axis2
        Deeper faxis2 ->
            Deeper
                $ map (map fillSizes)
                $ Axis.fillSizes faxis2


-- TODO: layout :: forall a. Size -> Flex Rule a -> Flex (Pos /\ Size) a -- Flex Unit (Pos /\ Size /\ a)
-- layout size = fit size >>> foldN >>> fillSizes



fold :: forall s a b. (Array (s /\ s) -> a -> b -> b) -> b -> Flex s a -> b
fold f def = case _ of
    Level axis ->
        Axis.fold2 (Array.singleton >>> f) def axis
    Deeper faxis ->
        Axis.fold2 (\pPos flex b -> fold (\cPos -> f (pPos : cPos)) b flex) def faxis


foldN :: forall n a b. Ring n => (Pos_ n -> Size_ n -> a -> b -> b) -> b -> Flex (Size_ n) a -> b
foldN = foldAt (zero <+> zero)
    where
        foldAt pPos f def (Level axis) =
            Axis.fold2N'
                (\cPos ->
                    f (pPos - cPos)
                )
                def
                axis
        foldAt pPos f def (Deeper faxis) =
            Axis.fold2N'
                (\cPos _ flex b ->
                    foldAt (pPos - cPos) f b flex
                )
                def
                faxis