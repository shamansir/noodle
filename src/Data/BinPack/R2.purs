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
import Data.Foldable (class Foldable, foldMap, foldM, foldr, foldl)
-- import Data.Functor (fmap)
import Data.Monoid
import Control.Alt ((<|>))
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Ord (class Ord)
import Data.Show (class Show)

data Bin2 n a
    = Node  { _width :: n, _height   :: n
            , _right :: Bin2 n a, _below :: Bin2 n a
            , _item  :: a }
    | Free  { _width :: n, _height :: n }

newtype Item n a = Item (n /\ n /\ a)

instance bin2Functor :: Functor (Bin2 n) where
    map f (Node w h r b e) = Node w h (map f r) (map f b) (f e)
    map _ (Free w h)       = Free w h

instance bin2Foldable :: Foldable (Bin2 n) where
    foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldr f def (Node _ _ r b e) = f e <> foldr f def r <> foldr f def b
    foldr _ def (Free _ _) = def
    foldl :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldl f def (Node _ _ r b e) = foldl f def b <> foldl f def r <> f e
    foldl _ def (Free _ _) = def
    foldMap f (Node _ _ r b e) = f e <> foldMap f r <> foldMap f b
    foldMap _ (Free _ _)       = mempty

container :: forall n a. n -> n -> Bin2 n a
container n n' = Free { _width : n, _height : n' }

sqContainer :: n -> Bin2 n a
sqContainer x = container x x

item :: forall n a. n -> n -> a -> Item n a
item w h i = Item $ w /\ h /\ i

node :: forall n a. n -> n -> Bin2 n a -> Bin2 n a -> a -> Bin2 n a
node w h pright pbelow i =
    Node { _width : w, _height : h, _right : pright, _below : pbelow, _item : i }

sqItem :: forall n a. n -> a -> Item n a
sqItem l = item l l

pack' :: forall n a. Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
pack' (Free { _width : fw, _height : fh }) (Item (w /\ h /\ i)) =
    let
        fits = w <= fw && h <= fh
        pright = container (fw - w) h
        pbelow = container fw (fh - h)
    in
        if fits then  Just $ node w h pright pbelow i else Nothing

pack' (Node { _width : nw, _height : nh, _right : r, _below : b, _item : ni }) i
    = pright i <|> pbelow i
    where
        pright = map (\r' -> node nw nh r' b ni) . pack' r
        pbelow = map (\b' -> node nw nh r b' ni) . pack' b

pack :: forall n a. Ord n => Bin2 n a -> List (Item n a) -> Maybe (Bin2 n a)
pack c = foldM pack' c . sortBy (comparing area)
    where
        area (Item (w /\ h /\ _)) = w * h

-- toList :: forall n a. Ord n => Bin2 n a -> List (a /\ (n /\ n /\ n /\ n))
toList = unpack' 0 0
    where
        unpack' _ _ (Free _)       = Nil
        unpack' x y (Node
            { _width : w, _height : h, _right : r, _below : b, _item : i }) =
            (i /\ (x /\ y /\ w /\ h)) : unpack' (x + w) y r <> unpack' x (y + h) b

sample :: forall n a. Ring n => Ord n => Bin2 n a -> n -> n -> Maybe (a /\ n /\ n)
sample (Free _)       _ _ = Nothing
sample (Node
            { _width : w, _height : h, _right : r, _below : b, _item : i })
        x y =
    case (compare x w /\ compare y h) of
        (LT /\ LT) -> Just (i /\ x /\ y)
        (_  /\ LT) -> sample r (x - w) y
        _        -> sample b x (y - h)
