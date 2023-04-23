module Data.KeyHolder where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Record.Xiaomian as X
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (SProxy(..))
import Prim.RowList as RL


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


data HoldPropsR :: forall k. (Symbol -> Type) -> k -> k -> Type
data HoldPropsR (p :: Symbol -> Type) x repr = HoldPropsR


data HoldPropsInOrder :: forall k. RL.RowList Type -> (Symbol -> Type) -> k -> Type
data HoldPropsInOrder rl (p :: Symbol -> Type) x = HoldPropsInOrder SOrder


data HoldPropsInOrderR :: forall k. RL.RowList Type -> (Symbol -> Type) -> k -> k -> Type
data HoldPropsInOrderR rl (p :: Symbol -> Type) x repr = HoldPropsInOrderR SOrder


class Holder (proxy :: Symbol -> Type) x where
  hold :: forall sym. IsSymbol sym => proxy sym -> x
  extract :: forall r. x -> (forall sym. IsSymbol sym => proxy sym -> r) -> r


class ReifyTo (trg :: Symbol -> Type) where
  reify :: forall sym. IsSymbol sym => Proxy sym -> trg sym


class ReifyOrderedTo (trg :: Symbol -> Type) where
  reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> trg sym


class Repr a repr where
  repr :: a -> repr


class ReifyWithReprTo (trg :: Symbol -> Type) repr where
  reifyR :: forall a sym. Repr a repr => IsSymbol sym => Proxy sym -> a -> trg sym /\ repr


class ReifyWithReprOrderedTo (trg :: Symbol -> Type) repr where
  reifyRAt :: forall a sym. Repr a repr => IsSymbol sym => Int -> Proxy sym -> a -> trg sym /\ repr


-- newtype IdxTag x = IdxTagged (Int /\ x)


instance holdKeysI ::
  (IsSymbol sym, Holder p x, ReifyTo p) =>
  FoldingWithIndex (HoldProps p x) (Proxy sym) (Array x) a (Array x) where
  foldingWithIndex HoldProps prop symbols _ =
    symbols <> [ hold (reify prop :: p sym) ]


instance holdKeysIR ::
  (IsSymbol sym, Holder p x, ReifyWithReprTo p repr, Repr a repr) =>
  FoldingWithIndex (HoldPropsR p x repr) (Proxy sym) (Array (x /\ repr)) a (Array (x /\ repr)) where
  foldingWithIndex HoldPropsR prop symbols a =
    symbols <> [
        case (reifyR prop a :: p sym /\ repr) of
          (p /\ repr) -> hold p /\ repr
    ]

instance holdKeysOrderedI ::
  (IsSymbol sym, Holder p x, ReifyOrderedTo p) =>
  FoldingWithIndex (HoldPropsInOrder rl p x) (Proxy sym) (Array (Int /\ x)) a (Array (Int /\ x)) where
  foldingWithIndex (HoldPropsInOrder order) prop symbols _ =
    Array.insertBy cmpF (index /\ hold (reifyAt index prop :: p sym)) symbols
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order prop


instance holdKeysOrderedIR ::
  (IsSymbol sym, Holder p x, ReifyWithReprOrderedTo p repr, Repr a repr) =>
  FoldingWithIndex (HoldPropsInOrderR rl p x repr) (Proxy sym) (Array (Int /\ x /\ repr)) a (Array (Int /\ x /\ repr)) where
  foldingWithIndex (HoldPropsInOrderR order) prop symbols a =
    Array.insertBy cmpF (index /\
      case (reifyRAt index prop a :: p sym /\ repr) of
          (p /\ repr) -> hold p /\ repr
    ) symbols
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order prop


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
  :: forall (r :: Row Type) rl p x.
  Holder p x =>
  ReifyOrderedTo p =>
  RL.RowToList r rl =>
  HFoldlWithIndex (HoldPropsInOrder rl p x) (Array (Int /\ x)) { | r } (Array (Int /\ x)) =>
  Proxy p ->
  SOrder ->
  { | r } ->
  (Array x)
orderedKeys _ order r =
  Tuple.snd <$> hfoldlWithIndex (HoldPropsInOrder order :: HoldPropsInOrder rl p x) ([] :: Array (Int /\ x)) r


orderedKeysF
  :: forall (r :: Row Type) rl p x.
  Holder p x =>
  ReifyOrderedTo p =>
  RL.RowToList r rl =>
  HFoldlWithIndex (HoldPropsInOrder rl p x) (Array (Int /\ x)) { | r } (Array (Int /\ x)) =>
  Proxy rl ->
  Proxy p ->
  SOrder ->
  { | r } ->
  (Array x)
orderedKeysF _ p  order r =
  orderedKeys p order r


{-
orderedKeysFromRow
  :: forall (row :: Row Type) p x rl (sym :: Symbol) keys.
  IsSymbol sym =>
  Holder p x =>
  ReifyOrderedTo p =>
  RL.RowToList keys rl =>
  HFoldlWithIndex (HoldPropsInOrder p x) (Array (Int /\ x)) { | row } (Array (Int /\ x)) =>
  -- HFoldlWithIndex (HoldPropsInOrder p x) (Array x) (SProxy sym) (Array x) =>
  Proxy p ->
  SOrder ->
  (Proxy row) ->
  (Array x)
orderedKeysFromRow proxy order _ =
  orderedKeys proxy order (Proxy :: Proxy ({ a :: SProxy "a", b :: SProxy "b", c :: SProxy "c" }))
-}

  -- orderedKeys proxy order (Proxy (RProxy ( a :: SProxy "a", b :: SProxy "b", c :: SProxy "c" )))
  -- orderedKeys proxy order $ (X.getKeysRecord' (Proxy :: _ (Record row) ) :: Record keys)


instance ReifyTo Proxy where
    reify :: forall sym. IsSymbol sym => Proxy sym -> Proxy sym
    reify = identity


instance ReifyTo SProxy where
    reify :: forall sym. IsSymbol sym => Proxy sym -> SProxy sym
    reify _ = SProxy


instance ReifyOrderedTo Proxy where
    reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> Proxy sym
    reifyAt _ = identity


instance ReifyOrderedTo SProxy where
    reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> SProxy sym
    reifyAt _ _ = SProxy


instance ReifyWithReprTo Proxy repr where
    reifyR :: forall a sym. Repr a repr => IsSymbol sym => Proxy sym -> a -> Proxy sym /\ repr
    reifyR _ a = Proxy /\ repr a


instance ReifyWithReprTo SProxy repr where
    reifyR :: forall a sym. Repr a repr => IsSymbol sym => Proxy sym -> a -> SProxy sym /\ repr
    reifyR _ a = SProxy /\ repr a


instance ReifyWithReprOrderedTo Proxy repr where
    reifyRAt :: forall a sym. Repr a repr => IsSymbol sym => Int -> Proxy sym -> a -> Proxy sym /\ repr
    reifyRAt _ _ a = Proxy /\ repr a


instance ReifyWithReprOrderedTo SProxy repr where
    reifyRAt :: forall a sym. Repr a repr => IsSymbol sym => Int -> Proxy sym -> a -> SProxy sym /\ repr
    reifyRAt _ _ a = SProxy /\ repr a


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


class IsSymbolsOrderH (order :: SOrder) x where
    reflectH :: Proxy order -> Array x


instance IsSymbolsOrderH SOrder.T x where
    reflectH _ = []
else instance (IsSymbol sym, Holder Proxy x, IsSymbolsOrderH tail x) => IsSymbolsOrderH (SOrder.TCons sym tail) x where
    reflectH _ = hold (Proxy :: _ sym) : reflectH (Proxy :: _ tail)


class Keys (xs :: RL.RowList Type) x where
  keysImpl :: Proxy xs -> Array x

instance nilKeys :: Keys RL.Nil x where
  keysImpl _ = mempty
else instance consKeys ::
  ( IsSymbol name
  , Holder Proxy x
  , Keys tail x
  ) => Keys (RL.Cons name ty tail) x where
  keysImpl _ = first : rest
    where
      first = hold (Proxy :: _ name)
      rest = keysImpl (Proxy :: _ tail)


class KeysO (xs :: RL.RowList Type) (proxy :: Symbol -> Type) x where
  keysImplO :: Proxy proxy -> SOrder -> Proxy xs -> Array (Int /\ x)

instance nilKeysO :: KeysO RL.Nil proxy x where
  keysImplO _ _ _ = mempty
else instance consKeysO ::
  ( IsSymbol name
  , Holder proxy x
  , ReifyOrderedTo proxy
  , KeysO tail proxy x
  ) => KeysO (RL.Cons name ty tail) proxy x where
  keysImplO :: forall xs. Proxy proxy -> SOrder -> Proxy xs -> Array (Int /\ x)
  keysImplO p order _ =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = hold (reifyAt index (Proxy :: Proxy name) :: proxy name)
      ordered = keysImplO p order (Proxy :: _ tail)


keys :: forall g row rl x
   . RL.RowToList row rl
  => Keys rl x
  => g row -- this will work for any type with the row as a param!
  -> Array x
keys _ = keysImpl (Proxy :: _ rl)


orderedKeys' :: forall g row rl proxy x
   . RL.RowToList row rl
  => KeysO rl proxy x
  => Proxy proxy
  -> SOrder
  -> g row -- this will work for any type with the row as a param!
  -> Array x
orderedKeys' p order _ = Tuple.snd <$> keysImplO p order (Proxy :: _ rl)
