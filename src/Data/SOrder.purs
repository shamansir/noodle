module Data.SOrder
    ( SOrder, TCons, T
    , Auto, Empty
    , auto, empty
    , class IsSymbolsOrder, reflect
    , class IsSymbolsOrderTL, reflectTL
    , class HasSymbolsOrder, instantiate, instantiateImpl
    , type (:::)
    , length, index, length', index'
    , sort, sortBy, sort', sortBy', sortL, sortByL, sortL', sortByL'
    -- , class HasOrder
    , s1, s2, s3, s4, s5, s6, s7, s8, s9
    , indexOf
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Tuple as Tuple
import Data.List as List
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (type (:*), D0, D1, D2, D3)
import Data.Typelevel.Num.Sets (class Pos, class Nat)
import Data.Vec (Vec, (+>))
import Data.Vec as Vec
import Data.Map (Map)
import Data.Map as Map
import Data.String as String

import Data.List (List)

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


instantiateImpl :: forall (order :: SOrder). IsSymbolsOrder order => Proxy order -> SOrder
instantiateImpl = reflect >>> Array.mapWithIndex (flip (/\)) >>> Map.fromFoldable >>> SOrder


type Test :: SOrder
type Test = ("foo" ::: "bar" ::: "lll" ::: T)


type Auto :: SOrder
type Auto = T


type Empty :: SOrder
type Empty = T


empty :: Proxy Empty
empty = Proxy


auto :: SOrder
auto = SOrder $ Map.empty


class HasSymbolsOrder :: SOrder -> Row Type -> Constraint
class IsSymbolsOrder order <= HasSymbolsOrder (order :: SOrder) rl where
    instantiate :: Proxy rl -> Proxy order -> SOrder
instance IsSymbolsOrder order => HasSymbolsOrder order a where
    instantiate = const $ instantiateImpl

-- instance HasOrder (order :: SOrder) a


-- TODO: to set & so on.
class IsSymbolsOrderTL (order :: SOrder) (n :: Type) where
    reflectTL :: Proxy order -> Vec n String


instance IsSymbolsOrderTL T D0 where
    reflectTL _ = Vec.empty
else instance (Succ ntail nres, IsSymbol sym, IsSymbolsOrderTL tail ntail) => IsSymbolsOrderTL (TCons sym tail) nres where
    reflectTL _ = reflectSymbol (Proxy :: _ sym) +> reflectTL (Proxy :: _ tail)


class IsSymbolsOrder (order :: SOrder) where
    reflect :: Proxy order -> Array String

instance IsSymbolsOrder T where
    reflect _ = []
else instance (IsSymbol sym, IsSymbolsOrder tail) => IsSymbolsOrder (TCons sym tail) where
    reflect _ = reflectSymbol (Proxy :: _ sym) : reflect (Proxy :: _ tail)
else instance IsSymbolsOrder (TCons sym tail) where
    reflect _ = []


length :: forall (order :: SOrder). IsSymbolsOrder order => Proxy order -> Int
length = reflect >>> Array.length


length' :: SOrder -> Int
length' (SOrder map) = Map.size map


index :: forall (order :: SOrder). IsSymbolsOrder order => Proxy order -> Int
index p = length p - 1


index' :: SOrder -> String -> Maybe Int
index' (SOrder map) = flip Map.lookup map


sort :: forall (order :: SOrder). IsSymbolsOrder order => Proxy order -> Array String -> Array String
sort p = sortBy p identity


sort' :: SOrder -> Array String -> Array String
sort' so =
    sortBy' so identity


sortL :: forall (order :: SOrder). IsSymbolsOrder order => Proxy order -> List String -> List String
sortL o =
    sortL' $ instantiateImpl o


sortL' :: SOrder -> List String -> List String
sortL' so =
    sortByL' so identity


sortBy :: forall (order :: SOrder) a. IsSymbolsOrder order => Proxy order -> (a -> String) -> Array a -> Array a
sortBy o =
    sortBy' $ instantiateImpl o


sortBy' :: forall a. SOrder -> (a -> String) -> Array a -> Array a
sortBy' (SOrder indexMap) toKey =
    Array.sortWith
        (\v ->
            case Map.lookup (toKey v) indexMap of
                Just n -> n
                Nothing -> top
        )


sortByL :: forall (order :: SOrder) a. Ord a => IsSymbolsOrder order => Proxy order -> (a -> String) -> List a -> List a
sortByL o =
    sortByL' $ instantiateImpl o


sortByL' :: forall a. Ord a => SOrder -> (a -> String) -> List a -> List a
sortByL' (SOrder indexMap) toKey =
    List.sortBy
        (\v1 v2 ->
            case indexOf v1 /\ indexOf v2 of
                Just n1 /\ Just n2 -> compare n1 n2
                _ -> compare v1 v2
        )
    where indexOf = flip Map.lookup indexMap <<< toKey


indexOf :: forall proxy sym. IsSymbol sym => SOrder -> proxy sym -> Int
indexOf order p = fromMaybe (-1) <<< index' order $ reflectSymbol p


instance Show SOrder where
    show (SOrder omap) = String.joinWith " ::: " $ map (\(idx /\ sym) -> show idx <> " " <> sym) $ Array.sortWith Tuple.fst $ map Tuple.swap $ Map.toUnfoldable omap


s1 :: forall proxy sym1. IsSymbol sym1 => proxy sym1 -> Proxy (sym1 ::: T)
s1 _ = Proxy


s2 :: forall proxy sym1 sym2. IsSymbol sym1 => IsSymbol sym2 => proxy sym1 -> proxy sym2 -> Proxy (sym1 ::: sym2 ::: T)
s2 _ _ = Proxy


s3 :: forall proxy sym1 sym2 sym3. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => proxy sym1 -> proxy sym2 -> proxy sym3 -> Proxy (sym1 ::: sym2 ::: sym3 ::: T)
s3 _ _ _ = Proxy


s4 :: forall proxy sym1 sym2 sym3 sym4. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: T)
s4 _ _ _ _ = Proxy


s5 :: forall proxy sym1 sym2 sym3 sym4 sym5. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => IsSymbol sym5 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> proxy sym5 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: sym5 ::: T)
s5 _ _ _ _ _ = Proxy


s6 :: forall proxy sym1 sym2 sym3 sym4 sym5 sym6. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => IsSymbol sym5 => IsSymbol sym6 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> proxy sym5 -> proxy sym6 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: sym5 ::: sym6 ::: T)
s6 _ _ _ _ _ _ = Proxy


s7 :: forall proxy sym1 sym2 sym3 sym4 sym5 sym6 sym7. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => IsSymbol sym5 => IsSymbol sym6 => IsSymbol sym7 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> proxy sym5 -> proxy sym6 -> proxy sym7 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: sym5 ::: sym6 ::: sym7 ::: T)
s7 _ _ _ _ _ _ _ = Proxy


s8 :: forall proxy sym1 sym2 sym3 sym4 sym5 sym6 sym7 sym8. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => IsSymbol sym5 => IsSymbol sym6 => IsSymbol sym7 => IsSymbol sym8 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> proxy sym5 -> proxy sym6 -> proxy sym7 -> proxy sym8 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: sym5 ::: sym6 ::: sym7 ::: sym8 ::: T)
s8 _ _ _ _ _ _ _ _ = Proxy


s9 :: forall proxy sym1 sym2 sym3 sym4 sym5 sym6 sym7 sym8 sym9. IsSymbol sym1 => IsSymbol sym2 => IsSymbol sym3 => IsSymbol sym4 => IsSymbol sym5 => IsSymbol sym6 => IsSymbol sym7 => IsSymbol sym8 => IsSymbol sym9 => proxy sym1 -> proxy sym2 -> proxy sym3 -> proxy sym4 -> proxy sym5 -> proxy sym6 -> proxy sym7 -> proxy sym8 -> proxy sym9 -> Proxy (sym1 ::: sym2 ::: sym3 ::: sym4 ::: sym5 ::: sym6 ::: sym7 ::: sym8 ::: sym9 ::: T)
s9 _ _ _ _ _ _ _ _ _ = Proxy
