module Type.Data.List.Extra where

import Prelude (class Applicative, pure)

import Type.Data.List (List', Cons', Nil', type (:>))
--import Int (Int)
import Type.Proxy (Proxy(..))
import Data.Array ((:))
-- import Data.Semigroup (class Semigroup, (<>))
-- import Data.Unfoldable (class Unfoldable)
-- import Data.Monoid (class Monoid, mempty)
import Data.Reflectable (class Reflectable, reflectType)


type TList :: forall k. k -> Type
type TList = List'


type TCons :: forall k. k -> TList k -> TList k
type TCons = Cons'


type TNil :: forall k. TList k
type TNil = Nil'


class Put :: forall k. k -> TList k -> TList k -> Constraint
class Put x xs ys | xs -> ys


instance Put x TNil (x :> TNil)
else instance Put x (some :> tail) (x :> some :> tail)


class MapDown :: forall k. TList k -> Type -> Constraint
class MapDown xs r | xs -> r where
    mapDown :: Proxy xs -> Array r


instance MapDown TNil r where
    mapDown :: Proxy TNil -> Array r
    mapDown _ = []
else instance (Reflectable x r, MapDown tail r) => MapDown (x :> tail) r where
    mapDown :: Proxy (x :> tail) -> Array r
    mapDown _ = reflectType (Proxy :: _ x) : mapDown (Proxy :: _ tail)


{-
class MapDown :: forall k. TList k -> Type -> Constraint
class MapDown xs r | xs -> r where
    mapDown :: Proxy xs -> r



instance Monoid r => MapDown TNil r where
    mapDown :: Proxy TNil -> r
    mapDown _ = mempty
else instance (Applicative (r x), Semigroup r, Reflectable x a, MapDown tail r) => MapDown (x :> tail) r where
    mapDown :: Proxy (x :> tail) -> r
    mapDown _ = pure (reflectType (Proxy :: _ x)) <> mapDown (Proxy :: _ tail)
-}



mapDownTest :: Array String
mapDownTest = mapDown (Proxy :: _ ("a" :> "b" :> "c" :> TNil))