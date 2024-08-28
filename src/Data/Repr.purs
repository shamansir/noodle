module Data.Repr
    ( Repr(..)
    , ensureTo, ensureFrom
    , class HasFallback, fallback, fallbackByRepr, fallbackBy
    , class ToRepr, toRepr
    , class FromRepr, fromRepr
    , class FromToRepr
    , exists, wrap, unwrap
    , class DataToReprRow, dataToReprRow, dataToReprRowBuilder
    , class FromReprRow, class FromReprRowBase, fromReprRow, fromReprRowBuilder
    , class ToReprRow, class ToReprRowBase, toReprRowBase
    , class DataFromToReprRow
    , class ReadRepr, readRepr
    , class WriteRepr, writeRepr
    , class ReadWriteRepr
    , fromMap, toMap
    , inbetween, inbetween'
    )
    where

import Prelude

import Debug as Debug

import Type.Proxy (Proxy(..))

import Data.Map (Map)
import Data.Map (lookup, insert, empty) as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
import Record.Extra (class Keys) as Record
import Record.Builder (Builder)
import Record.Builder as Builder


-- FIXME: Merge with `Node2.MapsFolds.Repr` and `Patch4.MapsFolds.Repr` and `Toolkit3.MapsFolds.Repr`.
-- FIXME: Maybe reorganize into `MapsFolds.Node2.Path` & `MapsFolds.Node2.Repr` and `MapsFolds.Patch4` and `MapsFolds.Toolkit3.Path` & `MapsFolds.Toolkit3.Repr`

{-
data Repr a
  = Accept a
  | Decline
-}


data Repr a
  = Repr a


instance Functor Repr where
    map f = unwrap >>> f >>> Repr


class HasFallback a where
    fallback :: a


class HasFallback repr <= ToRepr a repr where
    toRepr :: a -> Maybe (Repr repr)


class (HasFallback repr, HasFallback a) <= FromRepr repr a where
    fromRepr :: Repr repr -> Maybe a


class ReadRepr repr where
    readRepr :: String -> Maybe (Repr repr)


class WriteRepr repr where
    writeRepr :: Repr repr -> String


instance Show repr => Show (Repr repr) where
    show (Repr r) = show r


class (ReadRepr repr, WriteRepr repr) <= ReadWriteRepr repr
instance (ReadRepr repr, WriteRepr repr) => ReadWriteRepr repr


class (FromRepr repr a, ToRepr a repr) <= FromToRepr a repr
instance (FromRepr repr a, ToRepr a repr) => FromToRepr a repr


-- instance HasFallback x => ToRepr x x where toRepr = wrap >>> Just
-- instance HasFallback x => FromRepr x x where fromRepr = unwrap >>> Just
--instance Monoid a => HasFallback a where fallback = mempty


instance HasFallback Unit   where fallback = unit
instance HasFallback Int    where fallback = 0
instance HasFallback String where fallback = ""
instance ToRepr Unit Unit     where toRepr = wrap >>> Just
instance ToRepr Int Int       where toRepr = wrap >>> Just
instance ToRepr String String where toRepr = wrap >>> Just
instance FromRepr Unit Unit     where fromRepr = unwrap >>> Just
instance FromRepr Int Int       where fromRepr = unwrap >>> Just
instance FromRepr String String where fromRepr = unwrap >>> Just


-- wrap :: forall a. a -> Repr a
-- wrap = Repr

wrap :: forall repr. repr -> Repr repr
wrap = Repr


exists :: forall repr. repr -> Maybe (Repr repr)
exists = Just <<< wrap


unwrap :: forall repr. Repr repr -> repr
unwrap (Repr repr) = repr


ensureTo :: forall repr a. ToRepr a repr => a -> Repr repr
ensureTo = fromMaybe (Repr fallback) <<< toRepr


ensureFrom :: forall repr a. FromRepr repr a => Repr repr -> a
ensureFrom = fromMaybe fallback <<< fromRepr


fallbackByRepr :: forall repr. HasFallback repr => Maybe repr -> Repr repr
fallbackByRepr = fallbackBy Repr


fallbackBy:: forall a b. HasFallback a => (a -> b) -> Maybe a -> b
fallbackBy f = maybe (f fallback) f


class DataToReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class DataToReprRow rl row repr from to | rl -> row from to, repr -> row from to where
  dataToReprRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }


class DataFromToReprRow :: RL.RowList Type -> Row Type -> Type -> Constraint
class DataFromToReprRow rl row repr | rl -> row, repr -> row


class FromReprRowBase :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class FromReprRowBase rl row repr from to | rl -> row from to, repr -> row from to where
  fromReprRowBuilder :: Proxy repr -> Proxy rl -> Map String (Repr repr) -> Builder { | from } { | to }


class ToReprRowBase :: RL.RowList Type -> Row Type -> Type -> Type -> Constraint
class ToReprRowBase rl row k repr | rl -> row, repr -> row where
  toReprRowBase :: Proxy repr -> Proxy rl -> (forall field. IsSymbol field => Proxy field -> k) -> Record row -> Map k (Repr repr) -> Map k (Repr repr)


class (RL.RowToList row rl, Record.Keys rl, FromReprRowBase rl row repr () row) <= FromReprRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, FromReprRowBase rl row repr () row) => FromReprRow rl row repr

class (RL.RowToList row rl, Record.Keys rl, ToReprRowBase rl row k repr) <= ToReprRow rl row k repr
instance (RL.RowToList row rl, Record.Keys rl, ToReprRowBase rl row k repr) => ToReprRow rl row k repr


instance fromReprRowBaseNil :: FromReprRowBase RL.Nil row repr () () where
  fromReprRowBuilder _ _ _ = identity
else instance fromReprRowBaseCons ::
  ( IsSymbol name
  , HasFallback a
  , FromRepr repr a
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name a from' to
  , FromReprRowBase tail row repr from from'
  ) => FromReprRowBase (RL.Cons name a tail) row repr from to where
  fromReprRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: a) = fromMaybe fallback $ fromRepr =<< Map.lookup (reflectSymbol nameP) map
      rest = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


instance toReprRowBaseNil :: ToReprRowBase RL.Nil row k repr where
    toReprRowBase _ _ _ _ _ = Map.empty
else instance toReprRowBaseCons ::
  ( Ord k
  , IsSymbol name
  , ToRepr a repr
  , Row.Cons name a trash row
  , ToReprRowBase tail row k repr
  ) => ToReprRowBase (RL.Cons name a tail) row k repr where
    toReprRowBase prepr _ toKey rec prev =
      Map.insert (toKey nameP) value rest
      where
        nameP = Proxy :: _ name
        value = ensureTo $ R.get nameP rec
        rest = toReprRowBase prepr (Proxy :: _ tail) toKey rec prev


instance dataFromToReprRowNil :: DataFromToReprRow RL.Nil row repr
else instance dataFromToReprRowCons ::
  ( IsSymbol name
  , ToRepr a repr
  , FromRepr repr a
  , Row.Cons name a trash row
  , DataFromToReprRow tail row repr
  -- , Row.Lacks name from'
  -- , Row.Cons name (Maybe (Repr repr)) from' to
  ) => DataFromToReprRow (RL.Cons name a tail) row repr


instance dataToReprRowNil :: DataToReprRow RL.Nil row repr () () where
  dataToReprRowBuilder _ _ _ = identity
else instance dataToReprRowCons ::
  ( IsSymbol name
  , ToRepr a repr
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name (Maybe (Repr repr)) from' to
  , DataToReprRow tail row repr from from'
  ) => DataToReprRow (RL.Cons name a tail) row repr from to where
  dataToReprRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = toRepr $ R.get nameP rec
      rest = dataToReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


dataToReprRow :: forall row rl repr row'
   . RL.RowToList row rl
  => DataToReprRow rl row repr () row'
  => Record row
  -> Record row'
dataToReprRow r = Builder.build builder {}
  where
    builder = dataToReprRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) r


fromReprRow :: forall row rl repr
   . FromReprRow rl row repr
  => Map String (Repr repr) -> Record row
fromReprRow map = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) map


fromMap :: forall row rl repr
   . FromReprRow rl row repr
  => Map String (Repr repr) -> Record row
fromMap = fromReprRow


toMap :: forall k rl row repr
    .  ToReprRow rl row k repr
    => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k (Repr repr)
toMap toKey record = toReprRowBase (Proxy :: _ repr) (Proxy :: _ rl) toKey record Map.empty


inbetween :: forall a b reprA reprB. FromRepr reprA a => ToRepr b reprB => (a -> b) -> (reprA -> reprB)
inbetween f reprA = fromMaybe fallback $ unwrap <$> (toRepr =<< f <$> (fromRepr $ Repr reprA))


inbetween' :: forall a reprA reprB. FromRepr reprA a => ToRepr a reprB => Proxy a -> (reprA -> reprB)
inbetween' _ = inbetween (identity :: a -> a)