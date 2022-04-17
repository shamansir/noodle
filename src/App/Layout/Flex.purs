module App.Layout.Flex
  ( Flex
  , flex, nest
  --, fold, foldN
  , fit
  )
  where


import Prelude

import App.Style.Order (Order)
import Control.Apply (lift2)
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


flex :: forall s a. Array (s /\ Array (s /\ a)) -> Flex s a
flex = Level <<< Axis.make2


nest :: forall s a. Array (s /\ Array (s /\ Flex s a)) -> Flex s a
nest = Deeper <<< Axis.make2



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


-- TODO: layout :: forall a. Size -> Flex Rule a -> Flex (Pos /\ Size) a -- Flex Unit (Pos /\ Size /\ a)
-- layout size = fit size >>> foldN >>> fillSizes


data Dir = Horz | Vert

{-
foldN :: forall n a b. Semiring n => (Pos_ n -> Size_ n -> a -> b -> b) -> b -> Flex n a -> b
foldN = foldAt Vert (zero <+> zero) zero
    where
        foldAt dir pos n f def (Level axis) =
            fst $ case dir of
                Horz ->
                    Axis.fold
                        (\width a (b /\ prevX) ->
                            f ((V2.x pos + prevX) <+> V2.y pos) (width <+> n) a b /\ (prevX + width)
                        )
                        (def /\ zero)
                        axis
                Vert ->
                    Axis.fold
                        (\height a (b /\ prevY) ->
                            f (V2.x pos <+> (V2.y pos + prevY)) (n <+> height) a b /\ (prevY + height)
                        )
                        (def /\ zero)
                        axis
        foldAt dir pos n f def (Deeper faxis) =
            fst $ case dir of
                Horz ->
                    Axis.fold
                        (\width flex_ (b /\ prevX) ->
                            foldAt Vert ((V2.x pos + prevX) <+> V2.y pos) width f b flex_ /\ (prevX + width)
                        )
                        (def /\ zero)
                        faxis
                Vert ->
                    Axis.fold
                        (\height flex_ (b /\ prevY) ->
                            foldAt Horz (V2.x pos <+> (V2.y pos + prevY)) height f b flex_ /\ (prevY + height)
                        )
                        (def /\ zero)
                        faxis
-}