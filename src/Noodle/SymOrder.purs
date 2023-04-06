module Noodle.SymOrder
    ( SymOrder, TCons, T
    , class Index, index
    -- , order
    , class Length, length
    , class Values, values
    , type (:::)
    , class Orders, order
    , sort
    ) where

import Prelude

import Data.Const (Const)

import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Data.Array ((:))
import Data.Array (sortWith) as Array
-- import Data.Typelevel.Num.Reps (type (:*), D0, D1, D3) -- FIXME: use typelevel numbers? for quick calculations?

-- data TOrder
-- foreign import data TCons :: Type -> TOrder -> TOrder
-- foreign import data T :: Type -> TOrder


infixr 6 type TCons as :::


-- type Singleton :: TOrder
-- type Singleton = T String


-- type TwoItems :: TOrder
-- type TwoItems = String ::: T Int


-- type ThreeItems :: TOrder
-- type ThreeItems = Boolean ::: String ::: T Int



-- data SymOrder (k :: Symbol -> Type)
-- foreign import data TCons :: forall (k :: Symbol -> Type). (Symbol -> Type) -> SymOrder k -> SymOrder k
-- foreign import data T :: SymOrder


-- data SymOrder (n :: Type -> Type)
-- foreign import data TCons :: forall f n. Symbol -> SymOrder f -> SymOrder (n f)
-- foreign import data T :: SymOrder (Const D0)


data SymOrder
foreign import data TCons :: Symbol -> SymOrder -> SymOrder
foreign import data T :: SymOrder



type Test :: SymOrder
type Test = ("foo" ::: "bar" ::: "lll" ::: T)


class Length (order :: SymOrder) where
    length :: Proxy order -> Int


class Index (order :: SymOrder) where
    index :: Proxy order -> Int


class Values (order :: SymOrder) where
    values :: Proxy order -> Array String


instance Length T where
    length _ = 0

else instance Length tail => Length (TCons sym tail) where
    length _ = 1 + length (Proxy :: _ tail)


instance Index T where
    index _ = -1

else instance (Length tail, Index tail) => Index (TCons sym tail) where
    index _ = length (Proxy :: _ (TCons sym tail)) - 1


instance Values T where
    values _ = []

else instance (IsSymbol sym, Values tail) => Values (TCons sym tail) where
    values _ = reflectSymbol (Proxy :: _ sym) : values (Proxy :: _ tail)


class Orders (order :: SymOrder) a where
    order :: Proxy order -> a -> Int

instance Orders T String where
    order _ _ = -1

else instance (Index tail, Length tail, IsSymbol sym) => Orders (TCons sym tail) String where
    order _ val =
        if reflectSymbol (Proxy :: _ sym) == val then index (Proxy :: _ (TCons sym tail)) else -1



sort :: forall (order :: SymOrder). Orders order String => Proxy order -> Array String -> Array String
sort _ = Array.sortWith $ order (Proxy :: _ order)


-- sortedKeys :: Keys


{- class OrderBy (order :: SymOrder) a where
    orderBy :: Proxy order -> a -> a


instance orderBySingle :: IsSymbol sym => OrderBy T (Array String) where
    orderBy _ arr = arr

else instance orderByPair :: (IsSymbol symA, OrderBy tail (Array String)) => OrderBy (TCons symA tail) (Array String) where
    orderBy :: Proxy _ -> Array String -> Array String
    orderBy _ arr = arr <> orderBy (Proxy :: _ tail) arr -}
