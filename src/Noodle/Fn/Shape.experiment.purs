module Noodle.Fn.Shape.X where

import Prelude


-- import Noodle.Id (Inlet(..), Outlet, Temperament(..))

import Data.Map (Map)


infixr 6 type TCons as :::


foreign import data SKind :: Type


data Temperament
foreign import data Hot :: Temperament
foreign import data Cold :: Temperament


data InletId
     -- = InletId Int String
foreign import data Inlet :: Symbol -> Int -> Temperament -> Type -> InletId


data OutletId
     -- = OutletId Int String
foreign import data Outlet :: Symbol -> Int -> Type -> OutletId


type FooInlet :: InletId
type FooInlet = (Inlet "foo" 2 Hot Int)

-- _foo :: (Inlet "foo" 2 Int)?.l/


-- _foo :: InletId
-- _foo = InletId 2 "ax"


type Foo :: Inlets
type Foo = ICons (Inlet "foo" 2 Hot Int) (ICons (Inlet "bar" 3 Hot String) I)





data Inlets
foreign import data ICons :: InletId -> Inlets -> Inlets
foreign import data I :: Inlets


data Outlets
foreign import data OCons :: InletId -> Inlets -> Outlets
foreign import data O :: Outlets


-- type Order


data Order = Order (Map String Int)
foreign import data TCons :: Symbol -> Order -> Order
foreign import data T :: Order


-- _foo = [ (Inlet { order : 1, temp : Hot } :: Inlet "foo"), (Inlet { order : 2, temp : Hot } :: Inlet "bar") ]


type Test :: Order
type Test = ("foo" ::: "bar" ::: "lll" ::: T)




-- data Order' = Order' (Map String Int)
-- foreign import data TCons' :: forall (k :: Symbol -> Type). k -> Order -> Order
-- foreign import data T' :: Order

-- type Test' = ( (Inlet "foo") ::: T)