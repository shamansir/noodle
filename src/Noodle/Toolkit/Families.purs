module Noodle.Toolkit.Families where


import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TList, TNil)

import Data.Array ((:))

type Families = TList FamilyDef


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef



class MapFamilies (families :: Families) x | families -> x where
    -- mapFamily :: forall f state is os repr m x. Proxy families -> Proxy (F f state is os repr m) -> x
    mapFamilies :: Proxy families -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x


instance MapFamilies TNil x where  -- FIXME: it is just `Map` over `TList`
    mapFamilies :: Proxy TNil -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ _ = []
else instance (IsSymbol f, MapFamilies tail x) => MapFamilies (F f state is os repr m :> tail) x where
    mapFamilies :: Proxy (F f state is os repr m :> tail) -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ f = f (Proxy :: _ (F f state is os repr m)) : mapFamilies (Proxy :: _ tail) f