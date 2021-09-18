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


-- TODO: IsPinnableLayout instance


-- it's not the fair `Eq`, so we don't provide `Eq` typeclass instance here
compareByItem :: forall a. Eq a => Pin a -> Pin a -> Boolean
compareByItem a b = get a == get b


get :: forall a. Pin a -> a
get (Pin pin) = Tuple.fst pin


getNumbers :: forall a. Pin a -> (Pos /\ Size)
getNumbers (Pin (_ /\ pos /\ size)) = pos /\ size


empty :: forall a. PinBoard a
empty = []


pinNowhere :: forall a. a -> Pin a
pinNowhere i = Pin $ i /\ zero /\ zero


toArray :: forall a. PinBoard a -> Array (a /\ Pos /\ Size)
toArray = (<$>) (\(Pin t) -> t)


find :: forall a. Eq a => a -> PinBoard a -> Maybe (Pos /\ Size)
find needle pb =
    Array.find (compareByItem $ pinNowhere needle) pb
        <#> getNumbers


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
