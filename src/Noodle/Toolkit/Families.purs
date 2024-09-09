module Noodle.Toolkit.Families where


import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Data.Array ((:))

infixr 6 type FCons as //


data Families
foreign import data FCons :: FamilyDef -> Families -> Families
foreign import data FNil :: Families


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef


class FamilyExistsIn (family :: FamilyDef) (families :: Families)
instance FamilyExistsIn family (FCons family tail)
else instance (FamilyExistsIn family tail) => FamilyExistsIn family (FCons skipfamily tail)



class PutFamily (family :: FamilyDef) (families :: Families) (families' :: Families) | families -> families'


instance PutFamily family FNil (FCons family FNil)
else instance PutFamily family (FCons some tail) (FCons family (FCons some tail))


class MapFamilies (families :: Families) x | families -> x where
    -- mapFamily :: forall f state is os repr m x. Proxy families -> Proxy (F f state is os repr m) -> x
    mapFamilies :: Proxy families -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x


instance MapFamilies FNil x where
    mapFamilies :: Proxy FNil -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ _ = []
else instance (IsSymbol f, MapFamilies tail x) => MapFamilies (FCons (F f state is os repr m) tail) x where
    mapFamilies :: Proxy (FCons (F f state is os repr m) tail) -> (forall f state is os repr m. IsSymbol f => Proxy (F f state is os repr m) -> x) -> Array x
    mapFamilies _ f = f (Proxy :: _ (F f state is os repr m)) : mapFamilies (Proxy :: _ tail) f