module Layout.PinBoard where


import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2 (Pos, Size, (<+>))
import Data.Vec2 as V2
import Data.Newtype (unwrap, class Newtype)


import Web.Layout (class IsLayout, class IsSizedLayout, class IsContainerLayout, class IsPinningLayout)


newtype Pin a
    = Pin (a /\ Pos /\ Size)


newtype PinBoard a = PinBoard (Array (Pin a))


instance Functor Pin where
    map f (Pin (v /\ p /\ s)) = Pin $ f v /\ p /\ s


derive instance Functor PinBoard
derive instance Newtype (PinBoard a) _


instance IsLayout PinBoard where
    fold = fold
    find = find
    sample = sample


instance IsSizedLayout PinBoard where
    size = size

instance IsContainerLayout PinBoard where
    container _ = empty -- means can have any size and still work


instance IsPinningLayout PinBoard where
    pin v p s = pin p s v
    unpin = unpin


-- TODO: IsPinnableLayout instance


-- it's not the fair `Eq`, so we don't provide `Eq` typeclass instance here
compareByItem :: forall a. Eq a => Pin a -> Pin a -> Boolean
compareByItem a b = get a == get b


isPosInside :: forall a. Pos -> Pin a -> Maybe (a /\ Pos /\ Size)
isPosInside pos' (Pin (a /\ pos /\ size)) =
    if V2.inside pos' (pos /\ size) then
        --Just (a /\ pos' /\ size)
        Just (a /\ (pos' - pos) /\ size)
    else Nothing


size :: forall a. PinBoard a -> Size
size = toSize <<< fold addSize (V2.zero /\ V2.zero)
    where
        addSize (_ /\ pos /\ size) (minPos /\ maxPos) =
            (if pos < minPos then pos else minPos)
            /\
            (if pos + size > maxPos then pos + size else maxPos)
        toSize (minPos /\ maxPos) = maxPos - minPos


get :: forall a. Pin a -> a
get (Pin pin) = Tuple.fst pin


getNumbers :: forall a. Pin a -> (Pos /\ Size)
getNumbers (Pin (_ /\ pos /\ size)) = pos /\ size


empty :: forall a. PinBoard a
empty = PinBoard []


pinNowhere :: forall a. a -> Pin a
pinNowhere i = Pin $ i /\ zero /\ zero


toArray :: forall a. PinBoard a -> Array (a /\ Pos /\ Size)
toArray = unwrap >>> (<$>) (\(Pin t) -> t)


sample :: forall a. Pos -> PinBoard a -> Maybe (a /\ Pos /\ Size)
sample pos = Array.findMap (isPosInside pos) <<< unwrap


find :: forall a. Eq a => a -> PinBoard a -> Maybe (Pos /\ Size)
find needle pb =
    Array.find (compareByItem $ pinNowhere needle) (unwrap pb)
        <#> getNumbers


fold :: forall a k. ((a /\ Pos /\ Size) -> k -> k) -> k -> PinBoard a -> k
fold f k = toArray >>> Array.foldr f k



search :: forall a. Pos -> PinBoard a -> Maybe (a /\ Pos)
search pos = unwrap >>> Array.findMap matches
    where
        matches (Pin (item /\ ipos /\ size)) | V2.inside pos (ipos /\ size) = Just (item /\ (pos - ipos))
        matches _                            | otherwise = Nothing


search' :: forall a. Pos -> PinBoard a -> Maybe a
search' pos pb = Tuple.fst <$> search pos pb


pin :: forall a. Pos -> Size -> a -> PinBoard a -> PinBoard a
pin pos size i = unwrap >>> (:) (Pin $ i /\ pos /\ size) >>> PinBoard


unpin :: forall a. Eq a => a -> PinBoard a -> PinBoard a
-- unpin = Array.deleteBy compareByItem <<< pinNowhere
unpin a = unwrap >>> (Array.deleteBy compareByItem $ pinNowhere a) >>> PinBoard
