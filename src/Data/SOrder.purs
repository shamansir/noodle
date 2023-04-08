module Data.SOrder
    ( SOrder, TCons, T
    , class SymbolsHaveOrder, reflect
    , class SymbolsHaveOrderTL, reflectTL
    , type (:::)
    , length, index
    , sort, sortBy
    -- , class HasOrder
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (type (:*), D0, D1, D2, D3)
import Data.Typelevel.Num.Sets (class Pos, class Nat)
import Data.Vec (Vec, (+>))
import Data.Vec as Vec
import Data.Map (Map)
import Data.Map as Map

import Type.Proxy (Proxy(..))

import Record.Extra (class Keys, keysImpl)
import Type.RowList as RL

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



-- data SOrder (k :: Symbol -> Type)
-- foreign import data TCons :: forall (k :: Symbol -> Type). (Symbol -> Type) -> SOrder k -> SOrder k
-- foreign import data T :: SOrder


-- data SOrder (n :: Type -> Type)
-- foreign import data TCons :: forall f n. Symbol -> SOrder f -> SOrder (n f)
-- foreign import data T :: SOrder (Const D0)

newtype InOrder = InOrder String

data SOrder = SOrder {-(forall (n :: Type). Vec n String)-} (Map String Int)
foreign import data TCons :: Symbol -> SOrder -> SOrder
foreign import data T :: SOrder


instantiate :: forall (order :: SOrder). SymbolsHaveOrder order => Proxy order -> SOrder
instantiate = reflect >>> Array.mapWithIndex (flip (/\)) >>> Map.fromFoldable >>> SOrder


type Test :: SOrder
type Test = ("foo" ::: "bar" ::: "lll" ::: T)


-- class HasOrder (order :: SOrder) a

-- instance HasOrder (order :: SOrder) a


-- TODO: to set & so on.
class SymbolsHaveOrderTL (order :: SOrder) (n :: Type) where
    reflectTL :: Proxy order -> Vec n String


instance SymbolsHaveOrderTL T D0 where
    reflectTL _ = Vec.empty
else instance (Succ ntail nres, IsSymbol sym, SymbolsHaveOrderTL tail ntail) => SymbolsHaveOrderTL (TCons sym tail) nres where
    reflectTL _ = reflectSymbol (Proxy :: _ sym) +> reflectTL (Proxy :: _ tail)


class SymbolsHaveOrder (order :: SOrder) where
    reflect :: Proxy order -> Array String

instance SymbolsHaveOrder T where
    reflect _ = []
else instance (IsSymbol sym, SymbolsHaveOrder tail) => SymbolsHaveOrder (TCons sym tail) where
    reflect _ = reflectSymbol (Proxy :: _ sym) : reflect (Proxy :: _ tail)
else instance SymbolsHaveOrder (TCons sym tail) where
    reflect _ = []


length :: forall (order :: SOrder). SymbolsHaveOrder order => Proxy order -> Int
length = reflect >>> Array.length


index :: forall (order :: SOrder). SymbolsHaveOrder order => Proxy order -> Int
index p = length p - 1


sort :: forall (order :: SOrder). SymbolsHaveOrder order => Proxy order -> Array String -> Array String
sort p = sortBy p identity


sortBy :: forall (order :: SOrder) a. SymbolsHaveOrder order => Proxy order -> (a -> String) -> Array a -> Array a
sortBy o toKey what =
    let (SOrder indexMap) = instantiate o
    in Array.sortWith
        (\v ->
            case Map.lookup (toKey v) indexMap of
                Just n -> n
                Nothing -> top
        )
        what
