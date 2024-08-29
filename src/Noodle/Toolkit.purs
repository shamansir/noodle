module Noodle.Toolkit where

import Prelude
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Noodle.Node (Node)


data Toolkit
foreign import data TCons :: FamilyDef -> Toolkit -> Toolkit
foreign import data TNil :: Toolkit


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef



class FamilyInToolkit (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type) (nodes :: Toolkit)
instance FamilyInToolkit f state is os repr m (TCons (F f state is os repr m) tail)
else instance (FamilyInToolkit f state is os repr m tail) => FamilyInToolkit f state is os repr m (TCons (F skipf skipstate skipis skipos skiprepr skipm) tail)


type MyToolkit :: Toolkit
type MyToolkit =
    (TCons
        (F "a" Unit () () Int Effect)
        (TCons
            (F "foo" Unit ( a :: Int ) ( b :: String ) String Effect)
            TNil))


data Family (f :: Symbol) (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type) = Family


test :: forall f state is os repr m toolkit. FamilyInToolkit f state is os repr m toolkit => Proxy toolkit -> Family f state is os repr m -> String
test _ _ = "foo"


testFoo :: String
testFoo = test (Proxy :: _ MyToolkit) (Family :: Family "a" Unit () () Int Effect)


-- testFoo2 :: String
-- testFoo2 = test (Proxy :: _ TNil) (Family :: Family "a" Unit () () Int Effect)


-- testFoo3 :: String
-- testFoo3 = test (Proxy :: _ MyToolkit) (Family :: Family "foo" Unit () () Int Effect)


-- testFoo4 :: String
-- testFoo4 = test (Proxy :: _ MyToolkit) (Family :: Family "a" Unit ( ) () Int Effect)


testFoo5 :: String
testFoo5 = test (Proxy :: _ MyToolkit) (Family :: Family "foo" Unit ( a :: Int ) ( b :: String ) String Effect)