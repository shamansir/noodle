module Type.Data.List.Extra where

import Type.Data.List (List', Cons', Nil', type (:>))
--import Int (Int)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Array ((:))


import Data.Monoid (class Monoid, mempty)

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

{-

class MapDown :: forall k. k -> TList k -> Type -> Constraint
class MapDown a xs r | xs -> a, xs -> r where
    mapDown :: Proxy xs -> (Proxy a -> r) -> Array r


instance MapDown a TNil r where
    mapDown :: Proxy TNil -> (Proxy a -> r) -> Array r
    mapDown _ _ = []
else instance (MapDown x tail r) => MapDown x (x :> tail) r where
    mapDown :: Proxy (x :> tail) -> (Proxy x -> r) -> Array r
    mapDown _ f = f (Proxy :: _ x) : mapDown (Proxy :: _ tail) f



mapDownTest = mapDown (Proxy :: _ ("a" :> "b" :> "c" :> TNil)) reflectSymbol
-}