module Noodle.Toolkit.Families where

import Prelude

import Data.Map (Map)

import Type.Proxy (Proxy(..))
import Effect (Effect)

import Noodle.Id (InletR, OutletR)
import Noodle.Fn (Fn, RawFn, Process)
import Noodle.Fn.Raw.Process (RawProcess)


infixr 6 type FCons as //


data Families
foreign import data FCons :: FamilyDef -> Families -> Families
foreign import data FNil :: Families


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef



class FamilyExistsIn (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type) (families :: Families)
instance FamilyExistsIn f state is os repr m (FCons (F f state is os repr m) tail)
else instance (FamilyExistsIn f state is os repr m tail) => FamilyExistsIn f state is os repr m (FCons (F skipf skipstate skipis skipos skiprepr skipm) tail)


type MyFamilies :: Families
type MyFamilies
    =  F "a" Unit () () Int Effect
    // F "foo" Unit ( a :: Int ) ( b :: String ) String Effect
    // FNil


data Family (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type)
    = Family (Record is) (Record os) state (Process state is os repr m)


data RawFamily (state :: Type) (repr :: Type) (m :: Type -> Type)
    = RawFamily (Map InletR repr) (Map OutletR repr) state (RawProcess state repr m)


-- test :: forall f state is os repr m families. FamilyExistsIn f state is os repr m families => Proxy families -> Family f state is os repr m -> String
-- test _ _ = "foo"


-- testFoo :: String
-- testFoo = test (Proxy :: _ MyFamilies) (Family {} {} unit :: Family "a" Unit () () Int Effect)


-- testFoo2 :: String
-- testFoo2 = test (Proxy :: _ TNil) (Family :: Family "a" Unit () () Int Effect)


-- testFoo3 :: String
-- testFoo3 = test (Proxy :: _ MyToolkit) (Family :: Family "foo" Unit () () Int Effect)


-- testFoo4 :: String
-- testFoo4 = test (Proxy :: _ MyToolkit) (Family :: Family "a" Unit ( ) () Int Effect)


-- testFoo5 :: String
-- testFoo5 = test (Proxy :: _ MyFamilies) (Family { a : 5 } { b : "bar" } unit :: Family "foo" Unit ( a :: Int ) ( b :: String ) String Effect)