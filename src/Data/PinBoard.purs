module Data.PinBoard where


import Debug as Debug
import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple
import Data.Tuple.Nested (type (/\), (/\))


newtype Pin a
    = Pin (a /\ Number /\ Number /\ Number /\ Number)


type PinBoard a = Array (Pin a)


-- it's not the fair `Eq`, so we don't use `Eq` typeclass here
compareByItem :: forall a. Eq a => Pin a -> Pin a -> Boolean
compareByItem a b = get a == get b


get :: forall a. Pin a -> a
get (Pin pin) = Tuple.fst pin


empty :: forall a. PinBoard a
empty = []


pinNowhere :: forall a. a -> Pin a
pinNowhere i = Pin $ i /\ 0.0 /\ 0.0 /\ 0.0 /\ 0.0


toArray :: forall a. PinBoard a -> Array (a /\ Number /\ Number /\ Number /\ Number)
toArray = (<$>) (\(Pin t) -> t)


search :: forall a. (Number /\ Number) -> PinBoard a -> Maybe a
search (x /\ y) = Array.findMap matches
    where
        matches (Pin (item /\ ix /\ iy /\ iw /\ ih)) | x >= ix && x <= ix + iw && y >= iy && y <= iy + ih = Just item
        matches _                                    | otherwise = Nothing


pin :: forall a. (Number /\ Number) -> (Number /\ Number) -> a -> PinBoard a -> PinBoard a
pin (x /\ y) (w /\ h) i = (:) (Pin $ i /\ x /\ y /\ w /\ h)


unpin :: forall a. Eq a => a -> PinBoard a -> PinBoard a
unpin = Array.deleteBy compareByItem <<< pinNowhere
