module Data.PinBoard where


import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested as Tuple
import Data.Tuple.Nested (type (/\), (/\))


data Pin a
    = Pin (Number /\ Number /\ Number /\ Number /\ a)


type PinBoard a = Array (Pin a)


instance eqPin :: Eq a => Eq (Pin a) where
    eq a b = get a == get b


get :: forall a. Pin a -> a
get (Pin (_ /\ _ /\ _ /\ _ /\ i)) = i -- Tuple.get5


empty :: forall a. PinBoard a
empty = []


pinNowhere :: forall a. a -> Pin a
pinNowhere i = Pin $ 0.0 /\ 0.0 /\ 0.0 /\ 0.0 /\ i


search :: forall a. (Number /\ Number) -> PinBoard a -> Maybe a
search (x /\ y) = Array.findMap matches
    where
        matches (Pin (ix /\ iy /\ iw /\ ih /\ item)) | x >= ix && x <= iw && y >= iy && y <= ih = Just item
        matches _                                    | otherwise = Nothing


pin :: forall a. (Number /\ Number) -> (Number /\ Number) -> a -> PinBoard a -> PinBoard a
pin (x /\ y) (w /\ h) i = (:) (Pin $ x /\ y /\ w /\ h /\ i)


unpin :: forall a. Eq a => a -> PinBoard a -> PinBoard a
unpin = Array.delete <<< pinNowhere
