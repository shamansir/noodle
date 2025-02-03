module Type.Data.List.Extra where

import Prelude (class Applicative, pure)

import Type.Data.Peano as Peano
import Type.Data.List (List', Cons', Nil', type (:>), class Length, class IsMember)
import Type.Proxy (Proxy(..))
import Type.Data.Boolean (True)

-- import Prim.Boolean (True, False)
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


{-
-- | Adds an item to the end of `TList`, creating a new `TList`.
foreign import data TSnoc :: forall k. TList k -> k -> TList k


infixr 1 type TSnoc as <:
-}


class Put :: forall k. k -> TList k -> TList k -> Constraint
class Put x xs ys | xs -> ys


instance Put x TNil (x :> TNil)
else instance IsMember x (x :> some :> tail) True => Put x (some :> tail) (x :> some :> tail)


class Merge :: forall k. TList k -> TList k -> TList k -> Constraint
class Merge srcA srcB dst | srcA srcB -> dst


instance Merge TNil TNil TNil
else instance (Merge srcA srcB dst) => Merge (x :> srcA) srcB (x :> dst)
else instance (Merge srcA srcB dst) => Merge srcA (y :> srcB) (y :> dst)


{-
class Has :: forall k. k -> TList k -> Bool -> Constraint
class Has x xs (res :: Bool)


instance Has x TNil False
else instance Eq cA cB True => Has cA (cB :> TNil) True
else instance Has x (x :> some :> TNil) True
else instance Has x (some :> x :> TNil) True
else instance Has x tail res => Has x (y :> tail) res
-}


{-
class PutLast :: forall k. k -> TList k -> TList k -> Constraint
class PutLast x xs ys | xs -> ys


instance PutLast x TNil (x :> TNil)
else instance PutLast x before (before <: x)
-}


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
    mapDown key _ = pure (lmap key (Proxy :: _ x)) <> mapDown key (Proxy :: _ tail)