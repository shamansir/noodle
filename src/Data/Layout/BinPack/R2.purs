module Data.BinPack.R2
( Bin2
, Item
, pack
, packOne
, toList
, find
, sample
, sample'
, container
, sqContainer
, item
, sqItem
, itemOf
, valueOf
, size
, fold
, reflow
, reflow'
)
where

{- Based on: https://github.com/bflyblue/binpack/blob/master/Data/BinPack/R2.hs -}

import Prelude

import Control.Alt ((<|>))

import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldM)
import Data.List (List(..), (:), sortBy, singleton)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple as Tuple
import Data.Vec2 (Vec2_, (<+>), Pos_, Size_)
import Data.Vec2 as V2
import Data.Typelevel.Num.Reps (D2, d0, d1)


data Bin2 n a
    = Node  { w :: n, h :: n
            , r :: Bin2 n a, b :: Bin2 n a
            , i :: a }
    | Free  { w :: n, h :: n }

-- type DeepBin2 n a = Bin2 n { value :: a, inner :: Maybe (DeepBin2 n a) }

-- TODO: IsLayout instance


newtype Item n a = Item (Size_ n /\ a)

instance functorBin2 :: Functor (Bin2 n) where
    map f (Node { w, h, r, b, i }) = node (w <+> h) (map f r) (map f b) (f i)
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


container :: forall n a. Size_ n -> Bin2 n a
container size = Free { w : V2.w size, h : V2.h size }

sqContainer :: forall n a. n -> Bin2 n a
sqContainer x = container $ x <+> x

item :: forall n a. Size_ n -> a -> Item n a
item size i = Item $ size /\ i

node :: forall n a. Size_ n -> Bin2 n a -> Bin2 n a -> a -> Bin2 n a
node size pright pbelow item =
    Node { w : V2.w size, h : V2.h size, r : pright, b : pbelow, i : item }

sqItem :: forall n a. n -> a -> Item n a
sqItem l = item $ l <+> l

pack' :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
pack' (Free { w : fw, h : fh }) (Item (size /\ i)) =
    let
        w /\ h = V2.toTuple size
        fits = w <= fw && h <= fh
        pright = container $ fw - w <+> h
        pbelow = container $ fw <+> fh - h
    in
        if fits then Just $ node size pright pbelow i else Nothing

pack' (Node { w : nw, h : nh, r, b, i : ni }) i
    = pright i <|> pbelow i
    where
        pright = (<$>) (\r' -> node (nw <+> nh) r' b ni) <<< pack' r
        pbelow = (<$>) (\b' -> node (nw <+> nh) r b' ni) <<< pack' b

pack :: forall n a. Ring n => Ord n => Bin2 n a -> List (Item n a) -> Maybe (Bin2 n a)
pack c = foldM pack' c <<< sortBy (comparing area)
    where
        area (Item (size /\ _)) = V2.area size

packOne :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
packOne c = pack c <<< singleton

toList :: forall n a. Semiring n => Bin2 n a -> List (a /\ (Pos_ n /\ Size_ n))
toList = unpack' zero -- FIXME: use unfold for that
    where
        unpack' _   (Free _)                 = Nil
        unpack' pos (Node { w, h, r, b, i }) =
            (i /\ (pos /\ (w <+> h))) : unpack' (pos + V2.w' w) r <> unpack' (pos + V2.h' h) b

fold :: forall n a k. Semiring n => (a /\ (Pos_ n /\ Size_ n) -> k -> k) -> k -> Bin2 n a -> k
fold f =
    fold' zero
    where
        fold' :: Vec2_ n -> k -> Bin2 n a -> k
        fold' _   v (Free _)                 = v
        fold' pos v (Node { w, h, r, b, i }) =
            -- fold everything below, then at right, then the current item
            -- f (i /\ (x /\ y /\ w /\ h)) $ fold' (x + w) y (fold' x (y + h) v b) r
            -- fold everything at right, then below, then the current item
            f (i /\ (pos /\ (w <+> h)))
                $ fold'
                    (pos + V2.h' h)
                    (fold' (pos + V2.w' w) v r) b
                -- $ fold' x (y + h) (fold' (x + w) y v r) b

find :: forall n a. Eq a => Semiring n => a -> Bin2 n a -> Maybe (Pos_ n /\ Size_ n)
find needle =
    fold
        (\(item /\ (pos /\ size)) prev ->
            if isJust prev then prev
            else if (item == needle) then Just $ pos /\ size
            else Nothing
        )
        Nothing


sample :: forall n a. Ring n => Ord n => Bin2 n a -> Pos_ n -> Maybe (a /\ Pos_ n)
sample (Free _)                 _   = Nothing
sample (Node { w, h, r, b, i }) pos =
    case (compare (V2.x pos) w /\ compare (V2.y pos) h) of
        (LT /\ LT) -> Just $ i /\ pos
        (_  /\ LT) -> sample r (pos - V2.w' w)
        _          -> sample b (pos - V2.h' h)

sample' :: forall n a. Ring n => Ord n => Bin2 n a -> Pos_ n -> Maybe a
sample' bin pos = Tuple.fst <$> sample bin pos

valueOf :: forall a. Bin2 _ a -> Maybe a
valueOf (Free _) = Nothing
valueOf (Node { i }) = Just i

itemOf :: forall n a. Bin2 n a -> Maybe (a /\ Size_ n)
itemOf bin = valueOf bin >>= \v -> pure $ v /\ size bin

size :: forall n. Bin2 n _ -> Size_ n
size (Free { w, h }) = w <+> h
size (Node { w, h }) = w <+> h

reflow :: forall n a. Ring n => Ord n => Size_ n -> Bin2 n a -> Maybe (Bin2 n a)
reflow =
    fold
        (\(a /\ (_ /\ size)) dst ->
            dst >>= flip packOne (item size a)
        )
        <<< Just <<< container


reflow' :: forall n a. Ring n => Ord n => Size_ n -> Bin2 n a -> Bin2 n a
reflow' =
    fold
        (\(a /\ (_ /\ size)) dst ->
            packOne dst (item size a)
                # fromMaybe dst
        )
        <<< container