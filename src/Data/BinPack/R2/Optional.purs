module Data.BinPack.R2.Optional
    ( Bin2
    , Area
    , Item
    , pack
    , packOne
    , toList
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
    ) where


import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.BinPack.R2 as R2


data Area a -- isomorfic to Maybe
    = Taken a
    | Abandoned


type Bin2 n a =
    R2.Bin2 n (Area a)


type Item n a = R2.Item n (Area a)


container :: forall n a. n -> n -> Bin2 n a
container = R2.container


sqContainer :: forall n a. n -> Bin2 n a
sqContainer = R2.sqContainer


item :: forall n a. n -> n -> a -> Item n a
item w h i = R2.item w h $ Taken i


sqItem :: forall n a. n -> a -> Item n a
sqItem l = item l l


fromArea :: forall a. Area a -> Maybe a
fromArea (Taken a) = Just a
fromArea Abandoned = Nothing


-- TODO: tryFitToAbandoned :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
-- TODO: tryFitToAbandoned bin _ = Just bin


pack :: forall n a. Ring n => Ord n => Bin2 n a -> List (Item n a) -> Maybe (Bin2 n a)
pack = R2.pack

packOne :: forall n a. Ring n => Ord n => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
packOne = R2.packOne

toList :: forall n a. Semiring n => Bin2 n a -> List (a /\ (n /\ n /\ n /\ n))
toList = R2.toList >>> List.mapMaybe filterF
    where
        filterF (Taken a /\ bounds) = Just (a /\ bounds)
        filterF (Abandoned /\ _) = Nothing


unfold :: forall n a k. Semiring n => (a /\ (n /\ n /\ n /\ n) -> k -> k) -> k -> Bin2 n a -> k
unfold f = R2.unfold f'
    where
        f' (Abandoned /\ _) prev = prev
        f' (Taken a /\ s) prev = f (a /\ s) prev

sample :: forall n a. Ring n => Ord n => Bin2 n a -> n -> n -> Maybe (a /\ n /\ n)
sample bin x y = R2.sample bin x y >>= f'
    where
        f' (Abandoned /\ _ /\ _) = Nothing
        f' (Taken a /\ w /\ h) = Just (a /\ w /\ h)

sample' :: forall n a. Ring n => Ord n => Bin2 n a -> n -> n -> Maybe a
sample' bin x y = R2.sample' bin x y >>= fromArea

valueOf :: forall a. Bin2 _ a -> Maybe a
valueOf bin = R2.valueOf bin >>= fromArea

itemOf :: forall n a. Bin2 n a -> Maybe (a /\ n /\ n)
itemOf bin = R2.itemOf bin >>= f'
    where
        f' (Abandoned /\ _ /\ _) = Nothing
        f' (Taken a /\ w /\ h) = Just (a /\ w /\ h)

size :: forall n. Bin2 n _ -> n /\ n
size = R2.size

abandon :: forall n a. Eq a => a -> Bin2 n a -> Bin2 n a
abandon another = (<$>) abandonIt
    where
        abandonIt (Taken a) | a == another = Abandoned
        abandonIt (Taken a) | otherwise = Taken a
        abandonIt Abandoned = Abandoned