module Data.PinBoard where


import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2 (Pos, Size)
import Data.Vec2 as V2


newtype Pin a
    = Pin (a /\ Pos /\ Size)


type PinBoard a = Array (Pin a)


-- it's not the fair `Eq`, so we don't use `Eq` typeclass here
compareByItem :: forall a. Eq a => Pin a -> Pin a -> Boolean
compareByItem a b = get a == get b


get :: forall a. Pin a -> a
get (Pin pin) = Tuple.fst pin


empty :: forall a. PinBoard a
empty = []


pinNowhere :: forall a. a -> Pin a
pinNowhere i = Pin $ i /\ zero /\ zero


toArray :: forall a. PinBoard a -> Array (a /\ Pos /\ Size)
toArray = (<$>) (\(Pin t) -> t)


search :: forall a. Pos -> PinBoard a -> Maybe (a /\ Pos)
search pos = Array.findMap matches
    where
        matches (Pin (item /\ ipos /\ size)) | V2.inside pos (ipos /\ size) = Just (item /\ (pos - ipos))
        matches _                            | otherwise = Nothing


search' :: forall a. Pos -> PinBoard a -> Maybe a
search' pos pb = Tuple.fst <$> search pos pb


pin :: forall a. Pos -> Size -> a -> PinBoard a -> PinBoard a
pin pos size i = (:) (Pin $ i /\ pos /\ size)


unpin :: forall a. Eq a => a -> PinBoard a -> PinBoard a
unpin = Array.deleteBy compareByItem <<< pinNowhere
