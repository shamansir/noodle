module Type.Data.List.Extra where

import Prelude (class Applicative, pure)

import Type.Data.Peano as Peano
import Type.Data.List (List', Cons', Nil', type (:>), class Length)
import Type.Proxy (Proxy(..))

import Data.Tuple.Nested (type (/\), (/\))
import Data.Semigroup (class Semigroup, (<>))
import Data.Monoid (class Monoid, mempty)
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


class MapDown :: forall k. TList k -> (Type -> Type) -> Type -> Constraint
class MapDown xs f r | xs -> f where
    mapDown :: Proxy xs -> f r


instance Monoid (f r) => MapDown TNil f r where
    mapDown :: Proxy TNil -> f r
    mapDown _ = mempty
instance (Applicative f, Semigroup (f r), Reflectable x r, MapDown tail f r) => MapDown (x :> tail) f r where
    mapDown :: Proxy (x :> tail) -> f r
    mapDown _ = mapDown (Proxy :: _ tail) <> pure (reflectType (Proxy :: _ x))