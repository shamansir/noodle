module Noodle.ChRepr.ChChRepr
    ( ChRepr(..)
    , ensureTo, ensureFrom
    , fromEq, toEq
    , fallbackByChRepr
    , class ToChRepr, toChRepr
    , class FromChRepr, fromChRepr
    , class FromToChRepr
    , exists, wrap, unwrap
    , class DataToChReprRow, dataToChReprRow, dataToChReprRowBuilder
    , class FromChReprRow, class FromChReprRowBase, fromChReprRow, fromChReprRowBuilder
    , class ToChReprRow, class ToChReprRowBase, toChReprRowBase
    , class DataFromToChReprRow
    , class ReadChRepr, readChRepr
    , class WriteChRepr, writeChRepr
    , class ReadWriteChRepr
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

import Noodle.Repr.HasFallback (class HasFallback, fallback, fallbackBy)


-- FIXME: Merge with `Node2.MapsFolds.ChRepr` and `Patch4.MapsFolds.ChRepr` and `Toolkit3.MapsFolds.ChRepr`.
-- FIXME: Maybe reorganize into `MapsFolds.Node2.Path` & `MapsFolds.Node2.ChRepr` and `MapsFolds.Patch4` and `MapsFolds.Toolkit3.Path` & `MapsFolds.Toolkit3.ChRepr`


-- FIXME: There's a Generic class that is [almost] the same as `toChRepr` / `FromChRepr`: https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html


{-
data ChRepr a
  = Accept a
  | Decline
-}


data ChRepr a
  = ChRepr a


instance Functor ChRepr where
    map f = unwrap >>> f >>> ChRepr


class HasFallback repr <= ToChRepr a repr where
    toChRepr :: a -> Maybe (ChRepr repr)


class (HasFallback repr, HasFallback a) <= FromChRepr repr a where
    fromChRepr :: ChRepr repr -> Maybe a


class ReadChRepr repr where
    readChRepr :: String -> Maybe (ChRepr repr)


class WriteChRepr repr where
    writeChRepr :: ChRepr repr -> String


instance Show repr => Show (ChRepr repr) where
    show (ChRepr r) = show r


class (ReadChRepr repr, WriteChRepr repr) <= ReadWriteChRepr repr
instance (ReadChRepr repr, WriteChRepr repr) => ReadWriteChRepr repr


class (FromChRepr repr a, ToChRepr a repr) <= FromToChRepr a repr
instance (FromChRepr repr a, ToChRepr a repr) => FromToChRepr a repr


-- instance (HasFallback x, TypeEquals x x) => ToChRepr x x where toChRepr = wrap >>> Just
-- instance  (HasFallback x, TypeEquals x x) => FromChRepr x x where fromChRepr = unwrap >>> Just
--instance Monoid a => HasFallback a where fallback = mempty


instance ToChRepr Unit Unit     where toChRepr = toEq
instance ToChRepr Int Int       where toChRepr = toEq
instance ToChRepr String String where toChRepr = toEq
instance FromChRepr Unit Unit     where fromChRepr = fromEq
instance FromChRepr Int Int       where fromChRepr = fromEq
instance FromChRepr String String where fromChRepr = fromEq


-- wrap :: forall a. a -> ChRepr a
-- wrap = ChRepr

fromEq :: forall a. ChRepr a -> Maybe a
fromEq = unwrap >>> to >>> Just


toEq :: forall a. a -> Maybe (ChRepr a)
toEq = wrap >>> to >>> Just


wrap :: forall repr. repr -> ChRepr repr
wrap = ChRepr


exists :: forall repr. repr -> Maybe (ChRepr repr)
exists = Just <<< wrap


unwrap :: forall repr. ChRepr repr -> repr
unwrap (ChRepr repr) = repr


ensureTo :: forall repr a. ToChRepr a repr => a -> ChRepr repr
ensureTo = fromMaybe (ChRepr fallback) <<< toChRepr


ensureFrom :: forall repr a. FromChRepr repr a => ChRepr repr -> a
ensureFrom = fromMaybe fallback <<< fromChRepr


fallbackByChRepr :: forall repr. HasFallback repr => Maybe repr -> ChRepr repr
fallbackByChRepr = fallbackBy ChRepr


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


class (RL.RowToList row rl, Record.Keys rl, FromChReprRowBase rl row repr () row) <= FromChReprRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, FromChReprRowBase rl row repr () row) => FromChReprRow rl row repr

class (RL.RowToList row rl, Record.Keys rl, ToChReprRowBase rl row k repr) <= ToChReprRow rl row k repr
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
      (val :: a) = fromMaybe fallback $ fromChRepr =<< Map.lookup (reflectSymbol nameP) map
      rest = fromChReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


instance toChReprRowBaseNil :: ToChReprRowBase RL.Nil row k repr where
    toChReprRowBase _ _ _ _ _ = Map.empty
else instance toChReprRowBaseCons ::
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
        value = ensureTo $ R.get nameP rec
        rest = toChReprRowBase prepr (Proxy :: _ tail) toKey rec prev


instance dataFromToChReprRowNil :: DataFromToChReprRow RL.Nil row repr
else instance dataFromToChReprRowCons ::
  ( IsSymbol name
  , ToChRepr a repr
  , FromChRepr repr a
  , Row.Cons name a trash row
  , DataFromToChReprRow tail row repr
  -- , Row.Lacks name from'
  -- , Row.Cons name (Maybe (ChRepr repr)) from' to
  ) => DataFromToChReprRow (RL.Cons name a tail) row repr


instance dataToChReprRowNil :: DataToChReprRow RL.Nil row repr () () where
  dataToChReprRowBuilder _ _ _ = identity
else instance dataToChReprRowCons ::
  ( IsSymbol name
  , ToChRepr a repr
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name (Maybe (ChRepr repr)) from' to
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


inbetween :: forall a b reprA reprB. FromChRepr reprA a => ToChRepr b reprB => (a -> b) -> (reprA -> reprB)
inbetween f reprA = fromMaybe fallback $ unwrap <$> (toChRepr =<< f <$> (fromChRepr $ ChRepr reprA))


inbetween' :: forall a reprA reprB. FromChRepr reprA a => ToChRepr a reprB => Proxy a -> (reprA -> reprB)
inbetween' _ = inbetween (identity :: a -> a)