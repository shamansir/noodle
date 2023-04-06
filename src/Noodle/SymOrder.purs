module Noodle.SymOrder
    ( SymOrder, TCons, T
    , class Values, values
    , type (:::)
    , length, index
    , sort
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (type (:*), D0, D1, D2, D3)
import Data.Typelevel.Num.Sets (class Pos, class Nat)
import Data.Vec (Vec, (+>))
import Data.Vec as Vec
import Type.Proxy (Proxy(..))

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


class Values (order :: SymOrder) where
    values :: Proxy order -> Array String

instance Values T where
    values _ = []

else instance (IsSymbol sym, Values tail) => Values (TCons sym tail) where
    values _ = reflectSymbol (Proxy :: _ sym) : values (Proxy :: _ tail)


class VecValues (order :: SymOrder) n where
    vecValues :: Proxy order -> Vec n String


instance VecValues T D0 where
    vecValues _ = Vec.empty

else instance (Succ ntail nres, IsSymbol sym, VecValues tail ntail) => VecValues (TCons sym tail) nres where
    vecValues _ = reflectSymbol (Proxy :: _ sym) +> vecValues (Proxy :: _ tail)


length :: forall (order :: SymOrder). Values order => Proxy order -> Int
length = values >>> Array.length


index :: forall (order :: SymOrder). Values order => Proxy order -> Int
index p = length p - 1


sort :: forall (order :: SymOrder). Values order => Proxy order -> Array String -> Array String
sort o what =
    let vals = values o
    in Array.sortWith
        (\v ->
            -- FIXME: 0(n^2)
            case Array.elemIndex v vals of
                Just n -> n
                Nothing -> top
        )
        what


-- sortedKeys :: Keys


{- class OrderBy (order :: SymOrder) a where
    orderBy :: Proxy order -> a -> a


instance orderBySingle :: IsSymbol sym => OrderBy T (Array String) where
    orderBy _ arr = arr

else instance orderByPair :: (IsSymbol symA, OrderBy tail (Array String)) => OrderBy (TCons symA tail) (Array String) where
    orderBy :: Proxy _ -> Array String -> Array String
    orderBy _ arr = arr <> orderBy (Proxy :: _ tail) arr -}
