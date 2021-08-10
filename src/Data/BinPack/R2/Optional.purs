module Data.BinPack.R2.Optional
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
    , unfold
    , abandon
    , reflow
    , reflow'
    ) where


import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.BinPack.R2 as R2
import Data.Vec2 ((<+>), Pos_, Size_)


type Bin2 n a =
    R2.Bin2 n (Maybe a)


type Item n a = R2.Item n (Maybe a)


container :: forall n a. Size_ n -> Bin2 n a
container = R2.container


sqContainer :: forall n a. n -> Bin2 n a
sqContainer = R2.sqContainer


item :: forall n a. Size_ n -> a -> Item n a
item size i = R2.item size $ Just i


sqItem :: forall n a. n -> a -> Item n a
sqItem l = item $ l <+> l


-- TODO: tryFitToEmpty :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
-- TODO: tryFitToEmpty bin _ = Just bin


pack :: forall n a. Ring n => Ord n => Bin2 n a -> List (Item n a) -> Maybe (Bin2 n a)
pack = R2.pack

packOne :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
packOne = R2.packOne

toList :: forall n a. Semiring n => Bin2 n a -> List (a /\ (Pos_ n /\ Size_ n))
toList = R2.toList >>> List.mapMaybe filterF
    where
        filterF (Just a /\ bounds) = Just (a /\ bounds)
        filterF (Nothing /\ _)     = Nothing


unfold :: forall n a k. Semiring n => (a /\ (Pos_ n /\ Size_ n) -> k -> k) -> k -> Bin2 n a -> k
unfold f = R2.unfold f'
    where
        f' (Nothing /\ _) prev = prev
        f' (Just a /\ s) prev = f (a /\ s) prev

find :: forall n a. Eq a => Semiring n => a -> Bin2 n a -> Maybe (Pos_ n /\ Size_ n)
find needle = R2.find $ Just needle

sample :: forall n a. Ring n => Ord n => Bin2 n a -> Pos_ n -> Maybe (a /\ Size_ n)
sample bin pos = R2.sample bin pos >>= f'
    where
        f' (Nothing /\ _)   = Nothing
        f' (Just a /\ size) = Just (a /\ size)

sample' :: forall n a. Ring n => Ord n => Bin2 n a -> Pos_ n -> Maybe a
sample' bin pos = R2.sample' bin pos >>= identity

valueOf :: forall a. Bin2 _ a -> Maybe a
valueOf bin = R2.valueOf bin >>= identity

itemOf :: forall n a. Bin2 n a -> Maybe (a /\ Size_ n)
itemOf bin = R2.itemOf bin >>= f' -- lift?
    where
        f' (Nothing /\ _)   = Nothing
        f' (Just a /\ size) = Just (a /\ size)

size :: forall n. Bin2 n _ -> Size_ n
size = R2.size

abandon :: forall n a. Eq a => a -> Bin2 n a -> Bin2 n a
abandon another = (<$>) abandonIt
    where
        abandonIt (Just a) | a == another = Nothing
        abandonIt (Just a) | otherwise = Just a
        abandonIt Nothing = Nothing


reflow :: forall n a. Ring n => Ord n => Size_ n -> Bin2 n a -> Maybe (Bin2 n a)
reflow = R2.reflow

reflow' :: forall n a. Ring n => Ord n => Size_ n -> Bin2 n a -> Bin2 n a
reflow' = R2.reflow'