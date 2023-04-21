module Data.KeyHolder where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))


import Type.Proxy (Proxy)
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.SOrder (SOrder)
import Data.SOrder as SOrder

import Heterogeneous.Folding
    ( class HFoldlWithIndex
    , class FoldingWithIndex
    , hfoldlWithIndex
    )



data HoldProps :: forall k. (Symbol -> Type) -> k -> Type
data HoldProps (p :: Symbol -> Type) x = HoldProps


data HoldPropsInOrder :: forall k. (Symbol -> Type) -> k -> Type
data HoldPropsInOrder (p :: Symbol -> Type) x = HoldPropsInOrder SOrder


class Holder (proxy :: Symbol -> Type) x where
  hold :: forall sym. IsSymbol sym => proxy sym -> x
  extract :: forall r. x -> (forall sym. IsSymbol sym => proxy sym -> r) -> r


class ReifyTo (trg :: Symbol -> Type) where
  reify :: forall a sym. IsSymbol sym => Proxy sym -> a -> trg sym


class ReifyOrderedTo (trg :: Symbol -> Type) where
  reifyAt :: forall a sym. IsSymbol sym => Int -> Proxy sym -> a -> trg sym


newtype IdxTag x = IdxTagged (Int /\ x)


instance holdKeysI ::
  (IsSymbol sym, Holder p x, ReifyTo p) =>
  FoldingWithIndex (HoldProps p x) (Proxy sym) (Array x) a (Array x) where
  foldingWithIndex HoldProps prop symbols a =
    symbols <> [ hold (reify prop a :: p sym) ]


instance holdKeysOrderedI ::
  (IsSymbol sym, Holder p x, ReifyOrderedTo p) =>
  FoldingWithIndex (HoldPropsInOrder p x) (Proxy sym) (Array (Int /\ x)) a (Array (Int /\ x)) where
  foldingWithIndex (HoldPropsInOrder order) prop symbols a =
    Array.insertBy cmpF (index /\ hold (reifyAt index prop a :: p sym)) symbols
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = Maybe.fromMaybe (-1) $ SOrder.index' order $ reflectSymbol prop


holdKeys :: forall (r :: Row Type) p x.
  Holder p x => ReifyTo p => Proxy p ->
  HFoldlWithIndex (HoldProps p x) (Array x) { | r } (Array x) =>
  { | r } ->
  (Array x)
holdKeys _ r =
  hfoldlWithIndex (HoldProps :: HoldProps p x) ([] :: Array x) r


holdKeysFromRow :: forall (row :: Row Type) p x.
  Holder p x => ReifyTo p => Proxy p ->
  HFoldlWithIndex (HoldProps p x) (Array x) (Proxy row) (Array x) =>
  (Proxy row) ->
  (Array x)
holdKeysFromRow _ r =
  hfoldlWithIndex (HoldProps :: HoldProps p x) ([] :: Array x) r


orderedKeys
  :: forall (r :: Row Type) p x.
  Holder p x =>
  ReifyOrderedTo p =>
  HFoldlWithIndex (HoldPropsInOrder p x) (Array (Int /\ x)) { | r } (Array (Int /\ x)) =>
  Proxy p ->
  SOrder ->
  { | r } ->
  (Array x)
orderedKeys _ order r =
  Tuple.snd <$> hfoldlWithIndex (HoldPropsInOrder order :: HoldPropsInOrder p x) ([] :: Array (Int /\ x)) r


orderedKeysFromRow
  :: forall (row :: Row Type) p x.
  Holder p x =>
  ReifyOrderedTo p =>
  HFoldlWithIndex (HoldPropsInOrder p x) (Array x) (Proxy row) (Array x) =>
  Proxy p ->
  SOrder ->
  (Proxy row) ->
  (Array x)
orderedKeysFromRow _ order r =
  hfoldlWithIndex (HoldPropsInOrder order :: HoldPropsInOrder p x) ([] :: Array x) r


instance ReifyTo Proxy where
    reify :: forall a sym. IsSymbol sym => Proxy sym -> a -> Proxy sym
    reify p _ = p


instance ReifyTo SProxy where
    reify :: forall a sym. IsSymbol sym => Proxy sym -> a -> SProxy sym
    reify _ _ = SProxy


instance ReifyOrderedTo Proxy where
    reifyAt :: forall a sym. IsSymbol sym => Int -> Proxy sym -> a -> Proxy sym
    reifyAt _ p _ = p


instance ReifyOrderedTo SProxy where
    reifyAt :: forall a sym. IsSymbol sym => Int -> Proxy sym -> a -> SProxy sym
    reifyAt _ _ _ = SProxy


{-
holdKeysL :: forall r p x.
  Holder p x => ReifyTo p => Proxy p ->
  HFoldlWithIndex (HoldProps p x) (List x) { | r } (List x) =>
  { | r } ->
  (List x)
holdKeysL _ r =
  hfoldlWithIndex (HoldProps :: HoldProps p x) (List.Nil :: List x) r


orderedKeysL
  :: forall r p x.
  Holder p x =>
  ReifyOrderedTo p =>
  HFoldlWithIndex (HoldPropsInOrder p x) (Array x) { | r } (List x) =>
  Proxy p ->
  SOrder ->
  { | r } ->
  (List x)
orderedKeysL _ order r =
  hfoldlWithIndex (HoldPropsInOrder order :: HoldPropsInOrder p x) (List.Nil :: List x) r
-}