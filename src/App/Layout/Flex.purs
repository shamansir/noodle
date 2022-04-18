module App.Layout.Flex
  ( Flex
  , fit
  --, flex, put, nest
  , fold, foldN
  )
  where


import Prelude

import Data.Either (Either(..))
import Data.Array ((:))
-- import Data.Array.Ex ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, Pos_, (<+>))
import Data.Vec2 as V2

import App.Layout.Flex.Axis as Axis
import App.Layout.Flex.Axis (Axis2)
import App.Layout.Flex.Rule (Rule(..))


-- TODO: `IsLayout` instance (AutoSizedLayout?)

type Item s a = Either a (Flex s a)


data Flex s a
    = Flex (Axis2 s (Item s a))


-- TODO: Functor, etc.


{- flex :: forall s a. Array (s /\ Item s a) -> Flex s a
flex = Flex <<< Axis.make2


put :: forall s a. a -> Item s a
put = Left


nest :: forall s a. Array (s /\ Item s a) -> Item s a
nest = Right <<< flex -}


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
                Flex faxis2 ->
                    Flex
                        $ map (Axis.mapItems (\(cSize /\ axis2) -> cSize /\ map (map $ fitAt cSize) axis2))
                        -- $ map (Axis.mapItems (\(cSize /\ axis2) -> (pSize - cSize) /\ map (map $ fitAt cSize) axis2))
                        $ Axis.layout pSize faxis2


fillSizes :: forall a. Flex Number a -> Flex Size a
fillSizes (Flex faxis2) =
    Flex
        $ map (map $ map fillSizes)
        $ Axis.fillSizes faxis2


-- TODO: layout :: forall a. Size -> Flex Rule a -> Flex (Pos /\ Size) a -- Flex Unit (Pos /\ Size /\ a)
-- layout size = fit size >>> foldN >>> fillSizes



fold :: forall s a b. Semiring s => (Array (s /\ s) -> a -> b -> b) -> b -> Flex s a -> b
fold f def (Flex faxis2) =
    Axis.fold2
        (\pPos maybeItem b ->
            case maybeItem of
                Just (Left a) -> f [ map (fromMaybe zero) pPos ] a b -- FIXME
                Just (Right flex) ->
                    fold
                        (\cPos ->
                            f (map (fromMaybe zero) pPos : cPos) -- FIXME
                        )
                        b
                        flex
                Nothing -> b
        ) def faxis2


foldN :: forall n a b. Ring n => (Pos_ n -> Size_ n -> a -> b -> b) -> b -> Flex (Size_ n) a -> b
foldN = foldAt (zero <+> zero)
    where
        foldAt pPos f def (Flex faxis) =
            Axis.fold2S
                (\cPos cSize maybeItem b ->
                    case maybeItem of
                        Just (Left a) -> f (pPos - cPos) cSize a b
                        Just (Right flex) ->
                            foldAt (pPos - cPos) f b flex
                        Nothing -> b
                )
                def
                faxis