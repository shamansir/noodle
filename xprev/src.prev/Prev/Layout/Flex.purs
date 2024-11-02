module Prev.Layout.Flex
  ( Flex, Layers
  , fit, fitLayers
  , flex, flex1, put, putAll, nest, nest', nest1, nest2, nest2'
  , layers
  , fold, foldN, foldS
  , sizeS, sizeN
  )
  where


import Prelude

import Data.Either (Either(..))
import Data.Array ((:))
-- import Data.Array.Ex ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\), type (/\))
-- import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, Pos_, (<+>))

import Control.Alt ((<|>))

import Prev.Layout.Flex.Axis as Axis
import Prev.Layout.Flex.Axis (Axis2)
import Prev.Layout.Flex.Rule (Rule)

import Prev.Web.Layout (class IsLayout, class IsSizedLayout)
import Prev.Web.Layout as L


-- TODO: `IsLayout` instance (AutoSizedLayout?)

type Item s a = Either a (Flex s a)


data Flex s a
    = Flex (Axis2 s (Item s a))


data Layers s a
    = Layers (Array (Flex s a))


instance flexNIsLayout :: IsLayout (Flex Number) where
    fold f = foldN $ \pos size a -> f (a /\ pos /\ size)  -- FIXME: just make all the Layout's functions work take arguments instead of tuples (but return tuples still)
    find = posOfN
    sample pos l = atPosN pos l


instance flexSIsLayout :: IsLayout (Flex Size) where
    fold f = foldS $ \pos size a -> f (a /\ pos /\ size)  -- FIXME: just make all the Layout's functions work take arguments instead of tuples (but return tuples still)
    find = posOfS
    sample pos l = atPosS pos l


instance layersNIsLayout :: IsLayout (Layers Number) where
    fold f def (Layers layers) = Array.foldr (flip $ L.fold f) def layers
    find what (Layers layers) = Array.foldl (\prev layout -> prev <|> L.find what layout) Nothing layers
    sample pos (Layers layers) = Array.foldl (\prev layout -> prev <|> L.sample pos layout) Nothing layers


instance layersSIsLayout :: IsLayout (Layers Size) where
    fold f def (Layers layers) = Array.foldr (flip $ L.fold f) def layers
    find what (Layers layers) = Array.foldl (\prev layout -> prev <|> L.find what layout) Nothing layers
    sample pos (Layers layers) = Array.foldl (\prev layout -> prev <|> L.sample pos layout) Nothing layers


instance flexNIsSizedLayout :: IsSizedLayout (Flex Number) where
    size = sizeN


instance flexSIsSizedLayout :: IsSizedLayout (Flex Size) where
    size = sizeS


instance functorFlex :: Functor (Flex s) where
    map f (Flex axis2) = Flex $ map (map $ bimap f $ map f) axis2


instance bifunctorFlex :: Bifunctor Flex where
    bimap f g (Flex axis2) = Flex $ bimap f (bimap f $ bimap g $ bimap f g) axis2


-- instance layersIsLayout :: IsLayout (Layers Number)


flex :: forall s a. Array (s /\ Array (s /\ Item s a)) -> Flex s a
flex = Flex <<< Axis.make2 <<< map (map $ Just <<< map (map Just))


flex1 :: forall s a. s -> Array (s /\ Item s a) -> Flex s a
flex1 = curry (flex <<< Array.singleton)


layers :: forall s a. Array (Flex s a) -> Layers s a
layers = Layers


put :: forall s a. a -> Item s a
put = Left


putAll :: forall s a. Array (s /\ a) -> Array (s /\ Item s a)
putAll = map $ map put


nest :: forall s a. Array (s /\ Array (s /\ Item s a)) -> Item s a
nest = Right <<< flex


nest' :: forall s a. Flex s a -> Item s a
nest' = Right


nest1 :: forall s a. s -> Array (s /\ Item s a) -> Item s a
nest1 = curry (nest <<< Array.singleton)


nest2 :: forall s a. s -> Array (s /\ Array (s /\ Item s a)) -> Item s a
nest2 rule inner =
    nest
        [ rule /\
            [ rule /\
                nest inner
            ]
        ]


nest2' :: forall s a. s -> Flex s a -> Item s a
nest2' rule inner =
    nest
        [ rule /\
            [ rule /\
                nest' inner
            ]
        ]


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


atPosN :: forall n a. Semiring n => Ord n => Pos_ n -> Flex n a -> Maybe (a /\ Pos_ n /\ Size_ n)
atPosN pos (Flex axis2) = Nothing -- FIXME implement using `Axis.find'`


-- Vec D2 Number -> Flex (Vec D2 Number) a0 -> Maybe (Tuple a0 (Tuple (Vec D2 Number) (Vec D2 Number)))
atPosS :: forall n a. Semiring n => Ord n => Pos_ n -> Flex (Size_ n) a -> Maybe (a /\ Pos_ n /\ Size_ n)
atPosS pos (Flex axis2) = Nothing -- FIXME implement using `Axis.find'`


posOfN :: forall n a. Semiring n => Ord n => a -> Flex n a -> Maybe (Pos_ n /\ Size_ n)
posOfN a (Flex axis2) = Nothing -- FIXME implement using `Axis.posOf2`


posOfS :: forall n a. Semiring n => Ord n => a -> Flex (Size_ n) a -> Maybe (Pos_ n /\ Size_ n)
posOfS a (Flex axis2) = Nothing -- FIXME implement using `Axis.posOf2`


sizeN :: forall n a. Semiring n => Flex n a -> Size_ n
sizeN = const $ zero <+> zero -- FIXME: implement using `Axis.sizeOf`


sizeS :: forall n a. Semiring n => Flex (Size_ n) a -> Size_ n
sizeS = const $ zero <+> zero -- FIXME: implement using `Axis.sizeOf`


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


fitLayers :: forall a. Size -> Layers Rule a -> Layers Size a
fitLayers size (Layers layers) = Layers $ fit size <$> layers


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


foldS :: forall n a b. Ring n => (Pos_ n -> Size_ n -> a -> b -> b) -> b -> Flex (Size_ n) a -> b
foldS = foldAt (zero <+> zero)
    where
        foldAt pPos f def (Flex faxis) =
            Axis.fold2S
                (\cPos cSize maybeItem b ->
                    case maybeItem of
                        Just (Left a) -> f (pPos + cPos) cSize a b
                        Just (Right flex) ->
                            foldAt (pPos + cPos) f b flex
                        Nothing -> b
                )
                def
                faxis


foldN :: forall n a b. Ring n => (Pos_ n -> Size_ n -> a -> b -> b) -> b -> Flex n a -> b
foldN = foldAt (zero <+> zero)
    where
        foldAt pPos f def (Flex faxis) =
            Axis.fold2N
                (\cPos cSize maybeItem b ->
                    case maybeItem of
                        Just (Left a) -> f (pPos + cPos) cSize a b
                        Just (Right flex) ->
                            foldAt (pPos + cPos) f b flex
                        Nothing -> b
                )
                def
                faxis