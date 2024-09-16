module Noodle.Toolkit.Families where


import Prim.Boolean (True, False)

import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Reflectable (class Reflectable)

import Type.Data.List (type (:>), class IsMember)
import Type.Data.List.Extra (TList, TNil)

import Data.Array ((:))

import Noodle.Id (FamilyR, familyR)
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.HoldsFamily (HoldsFamily)

type Families = TList FamilyDef


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef


instance IsSymbol f => Reflectable (F f state is os repr m) FamilyR where
    reflectType :: Proxy (F f state is os repr m) -> FamilyR
    reflectType _ = familyR (Proxy :: _ f)


class RegisteredFamily :: FamilyDef -> Families -> Constraint
class RegisteredFamily family families


instance IsMember family families True => RegisteredFamily family families


{-
class MapFamilies (families :: Families) x | families -> x where
    -- mapFamily :: forall f state is os repr m x. Proxy families -> Proxy (F f state is os repr m) -> x
    mapFamilies :: Proxy families -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x


instance MapFamilies TNil x where  -- FIXME: it is just `Map` over `TList`
    mapFamilies :: Proxy TNil -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ _ = []
else instance (IsSymbol f, MapFamilies tail x) => MapFamilies (F f state is os repr m :> tail) x where
    mapFamilies :: Proxy (F f state is os repr m :> tail) -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ f = f (Proxy :: _ (F f state is os repr m)) : mapFamilies (Proxy :: _ tail) f
-}