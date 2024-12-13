module Noodle.Repr.ChReprNext
    ( ChRepr(..)
    , ensureFrom
    , accept, decline
    -- , fromEq, toEq
    -- , fallbackByRepr, fallbackBy
    , class ToChRepr, toChRepr
    , class FromChRepr, fromChRepr
    , class FromToChRepr
    -- , exists, wrap, unwrap
    , class DataToChReprRow, dataToChReprRow, dataToChReprRowBuilder
    , class FromChReprRow, class FromChReprRowBase, fromChReprRow, fromChReprRowBuilder
    , class ToChReprRow, class ToChReprRowBase, toChReprRowBase
    , class DataFromToChReprRow
    -- , class ReadRepr, readRepr
    -- , class WriteRepr, writeRepr
    -- , class ReadWriteRepr
    , fromMap, toMap
    , inbetween, inbetween'
    )
    where

import Prelude

import Type.Proxy (Proxy(..))
import Type.Equality (class TypeEquals, from, to)

import Data.Map (Map)
import Data.Map (lookup, insert, empty) as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
import Record.Extra (class Keys) as Record
import Record.Builder (Builder)
import Record.Builder as Builder


import Noodle.Repr.HasFallback (class HasFallback, fallback)


-- FIXME: There's a Generic class that is [almost] the same as `toChRepr` / `FromChRepr`: https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html


data ChRepr a
  = Accept a
  | Decline
  | Empty


instance Functor ChRepr where
    map f = case _ of
      Accept a -> Accept $ f a
      Decline -> Decline
      Empty -> Empty


class ToChRepr a repr where
    toChRepr :: a -> ChRepr repr


class FromChRepr repr a where
    fromChRepr :: repr -> Maybe a


{-
class ReadChRepr repr where
    readChRepr :: String -> Maybe (Repr repr)


class WriteChRepr repr where
    writeChRepr :: Repr repr -> String
-}


instance Show repr => Show (ChRepr repr) where
    show = case _ of
      Accept r -> "+" <> show r
      Decline -> "x"
      Empty -> "-"


{-
class    (ReadChRepr repr, WriteChRepr repr) <= ReadWriteChRepr repr
instance (ReadChRepr repr, WriteChRepr repr) => ReadWriteChRepr repr
-}


class    (FromChRepr repr a, ToChRepr a repr) <= FromToChRepr a repr
instance (FromChRepr repr a, ToChRepr a repr) => FromToChRepr a repr


-- instance (HasFallback x, TypeEquals x x) => ToChRepr x x where toRepr = wrap >>> Just
-- instance  (HasFallback x, TypeEquals x x) => FromChRepr x x where fromRepr = unwrap >>> Just
--instance Monoid a => HasFallback a where fallback = mempty


instance ToChRepr Unit Unit       where toChRepr = accept
instance ToChRepr Int Int         where toChRepr = accept
instance ToChRepr String String   where toChRepr = accept
instance FromChRepr Unit Unit     where fromChRepr = Just
instance FromChRepr Int Int       where fromChRepr = Just
instance FromChRepr String String where fromChRepr = Just


accept :: forall repr. repr -> ChRepr repr
accept = Accept


decline :: forall repr. ChRepr repr
decline = Decline


toMaybe :: forall repr. ChRepr repr -> Maybe repr
toMaybe = case _ of
  Accept a -> Just a
  Decline -> Nothing
  Empty -> Nothing


ensureFrom :: forall repr a. HasFallback a => FromChRepr repr a => ChRepr repr -> a
ensureFrom chRepr = fromMaybe fallback $ fromChRepr =<< toMaybe chRepr



{-
exists :: forall repr. repr -> Maybe (Repr repr)
exists = Just <<< wrap


unwrap :: forall repr. Repr repr -> repr
unwrap (Repr repr) = repr


ensureTo :: forall repr a. ToChRepr a repr => a -> Repr repr
ensureTo = fromMaybe (Repr fallback) <<< toRepr


ensureFrom :: forall repr a. FromChRepr repr a => Repr repr -> a
ensureFrom = fromMaybe fallback <<< fromRepr


fallbackByRepr :: forall repr. HasFallback repr => Maybe repr -> Repr repr
fallbackByRepr = fallbackBy Repr


fallbackBy:: forall a b. HasFallback a => (a -> b) -> Maybe a -> b
fallbackBy f = maybe (f fallback) f
-}


class DataToChReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class DataToChReprRow rl row repr from to | rl -> row from to, repr -> row from to where
  dataToChReprRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }


class DataFromToChReprRow :: RL.RowList Type -> Row Type -> Type -> Constraint
class DataFromToChReprRow rl row repr | rl -> row, repr -> row


class FromChReprRowBase :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class FromChReprRowBase rl row repr from to | rl -> row from to, repr -> row from to where
  fromChReprRowBuilder :: Proxy repr -> Proxy rl -> Map String (ChRepr repr) -> Builder { | from } { | to }


class ToChReprRowBase :: RL.RowList Type -> Row Type -> Type -> Type -> Constraint
class ToChReprRowBase rl row k repr | rl -> row, repr -> row where
  toChReprRowBase :: Proxy repr -> Proxy rl -> (forall field. IsSymbol field => Proxy field -> k) -> Record row -> Map k (ChRepr repr) -> Map k (ChRepr repr)


class    (RL.RowToList row rl, Record.Keys rl, FromChReprRowBase rl row repr () row) <= FromChReprRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, FromChReprRowBase rl row repr () row) => FromChReprRow rl row repr

class    (RL.RowToList row rl, Record.Keys rl, ToChReprRowBase rl row k repr) <= ToChReprRow rl row k repr
instance (RL.RowToList row rl, Record.Keys rl, ToChReprRowBase rl row k repr) => ToChReprRow rl row k repr


instance fromChReprRowBaseNil :: FromChReprRowBase RL.Nil row repr () () where
  fromChReprRowBuilder _ _ _ = identity
else instance fromChReprRowBaseCons ::
  ( IsSymbol name
  , HasFallback a
  , FromChRepr repr a
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name a from' to
  , FromChReprRowBase tail row repr from from'
  ) => FromChReprRowBase (RL.Cons name a tail) row repr from to where
  fromChReprRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: a) = fromMaybe fallback $ ensureFrom <$> Map.lookup (reflectSymbol nameP) map
      rest = fromChReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


instance toChReprRowBaseNil :: ToChReprRowBase RL.Nil row k repr where
    toChReprRowBase _ _ _ _ _ = Map.empty
else instance toReprRowBaseCons ::
  ( Ord k
  , IsSymbol name
  , ToChRepr a repr
  , Row.Cons name a trash row
  , ToChReprRowBase tail row k repr
  ) => ToChReprRowBase (RL.Cons name a tail) row k repr where
    toChReprRowBase prepr _ toKey rec prev =
      Map.insert (toKey nameP) value rest
      where
        nameP = Proxy :: _ name
        value = toChRepr $ R.get nameP rec
        rest = toChReprRowBase prepr (Proxy :: _ tail) toKey rec prev


instance dataFromToChReprRowNil :: DataFromToChReprRow RL.Nil row repr
else instance dataFromToChReprRowCons ::
  ( IsSymbol name
  , ToChRepr a repr
  , FromChRepr repr a
  , Row.Cons name a trash row
  , DataFromToChReprRow tail row repr
  -- , Row.Lacks name from'
  -- , Row.Cons name (Maybe (Repr repr)) from' to
  ) => DataFromToChReprRow (RL.Cons name a tail) row repr


instance dataToChReprRowNil :: DataToChReprRow RL.Nil row repr () () where
  dataToChReprRowBuilder _ _ _ = identity
else instance dataToChReprRowCons ::
  ( IsSymbol name
  , ToChRepr a repr
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name (ChRepr repr) from' to
  , DataToChReprRow tail row repr from from'
  ) => DataToChReprRow (RL.Cons name a tail) row repr from to where
  dataToChReprRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = toChRepr $ R.get nameP rec
      rest = dataToChReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


dataToChReprRow :: forall row rl repr row'
   . RL.RowToList row rl
  => DataToChReprRow rl row repr () row'
  => Record row
  -> Record row'
dataToChReprRow r = Builder.build builder {}
  where
    builder = dataToChReprRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) r


fromChReprRow :: forall row rl repr
   . FromChReprRow rl row repr
  => Map String (ChRepr repr) -> Record row
fromChReprRow map = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = fromChReprRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) map


fromMap :: forall row rl repr
   . FromChReprRow rl row repr
  => Map String (ChRepr repr) -> Record row
fromMap = fromChReprRow


toMap :: forall k rl row repr
    .  ToChReprRow rl row k repr
    => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k (ChRepr repr)
toMap toKey record = toChReprRowBase (Proxy :: _ repr) (Proxy :: _ rl) toKey record Map.empty


inbetween :: forall a b reprA reprB. HasFallback reprB => FromChRepr reprA a => ToChRepr b reprB => (a -> b) -> (reprA -> reprB)
inbetween f reprA = fromMaybe fallback $ (f <$> fromChRepr reprA) >>= (toChRepr >>> toMaybe)


inbetween' :: forall a reprA reprB. HasFallback reprB => FromChRepr reprA a => ToChRepr a reprB => Proxy a -> (reprA -> reprB)
inbetween' _ = inbetween (identity :: a -> a)