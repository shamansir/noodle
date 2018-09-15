module Data.BinPack.R2
( Bin2
, pack
, toList
, sample
, container
, sqContainer
, item
, sqItem
)
where

{- Based on: https://github.com/bflyblue/binpack/blob/master/Data/BinPack/R2.hs -}

import Prelude

import Control.Applicative
import Data.Foldable (class Foldable, foldMap, foldM)
-- import Data.Functor (fmap)
import Data.Monoid
import Control.Alt ((<|>))
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..))
import Data.List
import Data.Tuple.Nested ((/\), type (/\))
import Data.Ord (class Ord)
import Data.Show (class Show)

data Bin2 n a
    = Node  { _width :: n, _height   :: n
            , _right :: Bin2 n a, _below :: Bin2 n a
            , _item  :: a }
    | Free  { _width :: n, _height   :: n }

newtype Item n a = Item n n a

instance bin2Functor :: Functor (Bin2 n) where
    map f (Node w h r b e) = Node w h (map f r) (map f b) (f e)
    map _ (Free w h)       = Free w h

instance bin2Foldable :: Foldable (Bin2 n) where
    foldMap f (Node _ _ r b e) = f e <> foldMap f r <> foldMap f b
    foldMap _ (Free _ _)       = mempty

container :: n -> n -> Bin2 n a
container = Free

sqContainer :: n -> Bin2 n a
sqContainer x = container x x

item :: n -> n -> a -> Item n a
item w h i = Item w h i

sqItem :: n -> a -> Item n a
sqItem l = item l l

pack' :: forall n a. Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
pack' (Free fw fh) (Item w h i) =
    let
        fits = w <= fw && h <= fh
        pright = container (fw - w) h
        pbelow = container fw (fh - h)
    in
        if fits then  Just $ Node w h pright pbelow i else Nothing

pack' (Node nw nh r b ni) i = pright i <|> pbelow i
    where
        pright = map (\r' -> Node nw nh r' b ni) . pack' r
        pbelow = map (\b' -> Node nw nh r b' ni) . pack' b

pack :: forall n a. Ord n => Bin2 n a -> Array (Item n a) -> Maybe (Bin2 n a)
pack c = foldM pack' c . sortBy (comparing area)
    where
        area (Item w h _) = w * h

toList :: forall n a. Ord n => Bin2 n a -> Array (a /\ (n /\ n /\ n /\ n))
toList = unpack' 0 0
    where
        unpack' _ _ (Free _ _)       = []
        unpack' x y (Node w h r b i) =
            (i /\ (x /\ y /\ w /\ h)) : unpack' (x + w) y r <> unpack' x (y + h) b

sample :: forall n a. Ord n => Bin2 n a -> n -> n -> Maybe (a /\ n /\ n)
sample (Free _ _)       _ _ = Nothing
sample (Node w h r b i) x y =
    case (compare x w /\ compare y h) of
        (LT /\ LT) -> Just (i /\ x /\ y)
        (_  /\ LT) -> sample r (x - w) y
        _        -> sample b x (y - h)
