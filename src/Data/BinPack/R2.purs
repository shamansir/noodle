module Data.BinPack.R2
( Bin2
, Item
, pack
, packOne
, toList
, sample
, container
, sqContainer
, item
, sqItem
, itemOf
, valueOf
, size
, unfold
)
where

{- Based on: https://github.com/bflyblue/binpack/blob/master/Data/BinPack/R2.hs -}

import Prelude

import Control.Alt ((<|>))

import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldM)
import Data.List (List(..), (:), sortBy, singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

data Bin2 n a
    = Node  { w :: n, h :: n
            , r :: Bin2 n a, b :: Bin2 n a
            , i :: a }
    | Free  { w :: n, h :: n }

-- type DeepBin2 n a = Bin2 n { value :: a, inner :: Maybe (DeepBin2 n a) }

newtype Item n a = Item (n /\ n /\ a)

instance functorBin2 :: Functor (Bin2 n) where
    map f (Node { w, h, r, b, i }) = node w h (map f r) (map f b) (f i)
    map _ (Free { w, h })          = Free { w, h }

instance foldableBin2 :: Foldable (Bin2 n) where
    foldr f e (Node { r, b, i }) =
        f i $ foldr f (foldr f e b) r
    foldr _ e (Free _) = e
    foldl f e (Node { r, b, i }) =
        foldl f (foldl f (f e i) r) b
    foldl f e (Free _) = e
    foldMap f (Node { r, b, i }) = f i <> foldMap f r <> foldMap f b
    foldMap _ (Free _)           = mempty

instance showBin2 :: (Show a, Show n) => Show (Bin2 n a) where
    show (Node { r, b, i, w, h }) =
        "{ Node: " <> show w <> "x" <> show h <> " -> "
            <> show i <> " >> " <> show r <> "\n" <> show b <> " }"
    show (Free { w, h }) = "{ Free: " <> show w <> "x" <> show h <> " }"


container :: forall n a. n -> n -> Bin2 n a
container w h = Free { w, h }

sqContainer :: forall n a. n -> Bin2 n a
sqContainer x = container x x

item :: forall n a. n -> n -> a -> Item n a
item w h i = Item $ w /\ h /\ i

node :: forall n a. n -> n -> Bin2 n a -> Bin2 n a -> a -> Bin2 n a
node width height pright pbelow item =
    Node { w : width, h : height, r : pright, b : pbelow, i : item }

sqItem :: forall n a. n -> a -> Item n a
sqItem l = item l l

pack' :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
pack' (Free { w : fw, h : fh }) (Item (w /\ h /\ i)) =
    let
        fits = w <= fw && h <= fh
        pright = container (fw - w) h
        pbelow = container fw (fh - h)
    in
        if fits then Just $ node w h pright pbelow i else Nothing

pack' (Node { w : nw, h : nh, r, b, i : ni }) i
    = pright i <|> pbelow i
    where
        pright = (<$>) (\r' -> node nw nh r' b ni) <<< pack' r
        pbelow = (<$>) (\b' -> node nw nh r b' ni) <<< pack' b

pack :: forall n a. Ring n => Ord n => Bin2 n a -> List (Item n a) -> Maybe (Bin2 n a)
pack c = foldM pack' c <<< sortBy (comparing area)
    where
        area (Item (w /\ h /\ _)) = w * h

packOne :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
packOne c = pack c <<< singleton

toList :: forall n a. Semiring n => Bin2 n a -> List (a /\ (n /\ n /\ n /\ n))
toList = unpack' zero zero -- FIXME: use unfold for that
    where
        unpack' _ _ (Free _)       = Nil
        unpack' x y (Node { w, h, r, b, i }) =
            (i /\ (x /\ y /\ w /\ h)) : unpack' (x + w) y r <> unpack' x (y + h) b

unfold :: forall n a k. Semiring n => (a /\ (n /\ n /\ n /\ n) -> k -> k) -> k -> Bin2 n a -> k
unfold f =
    unfold' zero zero
    where
        unfold' _ _ v (Free _)       = v
        unfold' x y v (Node { w, h, r, b, i }) =
            -- unfold everything below, then at right, then the current item
            -- f (i /\ (x /\ y /\ w /\ h)) $ unfold' (x + w) y (unfold' x (y + h) v b) r
            -- unfold everything at right, then below, then the current item
            f (i /\ (x /\ y /\ w /\ h))
                $ unfold' x (y + h) (unfold' (x + w) y v r) b

sample :: forall n a. Ring n => Ord n => Bin2 n a -> n -> n -> Maybe (a /\ n /\ n)
sample (Free _)       _ _ = Nothing
sample (Node { w, h, r, b, i }) x y =
    case (compare x w /\ compare y h) of
        (LT /\ LT) -> Just (i /\ x /\ y)
        (_  /\ LT) -> sample r (x - w) y
        _          -> sample b x (y - h)

valueOf :: forall a. Bin2 _ a -> Maybe a
valueOf (Free _) = Nothing
valueOf (Node { i }) = Just i

itemOf :: forall n a. Bin2 n a -> Maybe (a /\ n /\ n)
itemOf bin = valueOf bin >>= \v -> pure $ v /\ size bin

size :: forall n. Bin2 n _ -> n /\ n
size (Free { w, h }) = w /\ h
size (Node { w, h }) = w /\ h

repack :: forall n a. n -> n -> Bin2 n a -> Bin2 n a
repack newWidth newHeight bin = bin -- FIXME: implement
