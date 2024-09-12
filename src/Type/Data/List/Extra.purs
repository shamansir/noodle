module Type.Data.List.Extra where

import Prelude (class Applicative, pure)

import Type.Data.Peano as Peano
import Type.Data.List (List', Cons', Nil', type (:>), class Length, class IsMember)
import Type.Proxy (Proxy(..))

import Prim.Boolean (True, False)
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


data ByReflect = ByReflect


class LMap :: forall k. Type -> k -> Type -> Constraint
class LMap key src trg where
    lmap :: key -> Proxy src -> trg


instance (Reflectable src trg) => LMap ByReflect src trg where
    lmap :: ByReflect -> Proxy src -> trg
    lmap _ psrc = reflectType psrc


class MapDown :: forall k. Type -> TList k -> (Type -> Type) -> Type -> Constraint
class MapDown key xs f r | xs -> f where
    mapDown :: key -> Proxy xs -> f r


instance Monoid (f r) => MapDown key TNil f r where
    mapDown :: key -> Proxy TNil -> f r
    mapDown _ = mempty
instance (Applicative f, Semigroup (f r), LMap key x r, MapDown key tail f r) => MapDown key (x :> tail) f r where
    mapDown :: key -> Proxy (x :> tail) -> f r
    mapDown key _ = mapDown key (Proxy :: _ tail) <> pure (lmap key (Proxy :: _ x))