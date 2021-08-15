module App.Style.Order where


import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Unfoldable (class Unfoldable)


type SizeF a = (a -> Number)


newtype Order a = Order (OSet a)


make :: forall f. Foldable f => f ~> Order
make = fromFoldable


fromFoldable :: forall f. Foldable f => f ~> Order
fromFoldable = Order <<< OSet.fromFoldable


-- make :: forall f a. Foldable f => (a -> Number) -> f a -> Order a
-- make sizeF = Order sizeF <<< OSet.fromFoldable


toUnfoldable :: forall f. Unfoldable f => Order ~> f
toUnfoldable (Order set) = OSet.toUnfoldable set


toArray :: Order ~> Array
toArray (Order set) = Array.fromFoldable set


toArray' :: forall a. SizeF a -> Order a -> Array (a /\ Number)
toArray' sizeF =
    map (\i -> i /\ sizeF i) <<< toArray

-- TODO: IsLayout instance


-- sum the sizes of items using given function
sizeBy :: forall a. SizeF a -> Order a -> Number
sizeBy sizeF (Order set) =
    foldr (+) 0.0 $ OSet.map sizeF set


-- sum the sizes before the item, or Nothing if the item wasn't met
sizeBefore :: forall a. SizeF a -> (a -> Boolean) -> Order a -> Maybe Number
sizeBefore sizeF cmp =
    foldl foldF Nothing <<< toArray' sizeF
    where
        foldF Nothing  (item /\ _)    | cmp item  = Just 0.0
        foldF Nothing  _              | otherwise = Nothing
        foldF (Just n) (_    /\ size)             = Just $ n + size


-- sum the sizes after the item, or Nothing if the item wasn't met
sizeAfter :: forall a. SizeF a -> (a -> Boolean) -> Order a -> Maybe Number
sizeAfter sizeF cmp =
    foldr foldF Nothing <<< toArray' sizeF
    where
        foldF (item /\ _   ) Nothing  | cmp item  = Just 0.0
        foldF  _             Nothing  | otherwise = Nothing
        foldF (_    /\ size) (Just n) = Just $ n + size


-- sum the sizes before the item and including the item or Nothing if the item wasn't met
sizeAt :: forall a. SizeF a -> (a -> Boolean) -> Order a -> Maybe Number
sizeAt sizeF cmp =
    foldl foldF Nothing <<< toArray' sizeF
    where
        foldF Nothing  (item /\ size) | cmp item  = Just size
        foldF Nothing  _              | otherwise = Nothing
        foldF (Just n) (_    /\ size)             = Just $ n + size