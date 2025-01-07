module Noodle.Repr.ChannelRepr
    ( ValueInChannel
    , accept, decline, empty
    , class ToValueInChannel, toValueInChannel
    , class FromValueInChannel, fromValueInChannel
    , class FromToValueInChannel
    -- , exists, wrap, unwrap
    -- , class DataToValueInChannelRow, dataToValueInChannelRow, dataToValueInChannelRowBuilder
    , class FromValueInChannelRow, class FromValueInChannelRowBase, fromValueInChannelRow, fromValueInChannelRowBuilder
    , class ToValueInChannelRow, class ToValueInChannelRowBase, toValueInChannelRowBase
    -- , class DataFromToValueInChannelRow
    , class ReadChannelRepr, readChannelRepr
    , class WriteChannelRepr, writeChannelRepr
    , class ReadWriteChannelRepr
    , fromMap, toMap
    -- , inbetween, inbetween'
    )
    where

import Prelude

import Data.Newtype (class Newtype)

import Type.Proxy (Proxy(..))
import Type.Equality (class TypeEquals, from, to)

import Data.Map (Map)
import Data.Map (lookup, insert, empty) as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
import Record.Extra (class Keys) as Record
import Record.Builder (Builder)
import Record.Builder as Builder

import Noodle.Repr.HasFallback (class HasFallback, fallback, fallbackBy)


{-
data ValueInChannel a
  = Accepted a
  | Declined { declined :: a, current :: a }
  | Empty
-}


data ValueInChannel a
  = Accepted a
  | Declined
  | Empty


derive instance Functor ValueInChannel



{- newtype Tag = Tag String


derive instance Newtype Tag _


class TagsValue a where
    tagValue :: a -> Tag
-}


class ToValueInChannel repr a where
    toValueInChannel :: repr -> ValueInChannel a


class FromValueInChannel a repr where
    fromValueInChannel :: a -> Maybe repr


class ReadChannelRepr repr where
    readChannelRepr :: String -> Maybe repr


class WriteChannelRepr repr where
    writeChannelRepr :: repr -> String


instance Show a => Show (ValueInChannel a) where
    show (Accepted r) = "✓ " <> show r
    show Declined = "𐄂"
    show Empty = "∅"


class    (ReadChannelRepr repr, WriteChannelRepr repr) <= ReadWriteChannelRepr repr
instance (ReadChannelRepr repr, WriteChannelRepr repr) => ReadWriteChannelRepr repr


class    (FromValueInChannel repr a, ToValueInChannel a repr) <= FromToValueInChannel a repr
instance (FromValueInChannel repr a, ToValueInChannel a repr) => FromToValueInChannel a repr


-- instance (HasFallback x, TypeEquals x x) => ToChannelRepr x x where toChannelRepr = wrap >>> Just
-- instance (HasFallback x, TypeEquals x x) => FromChannelRepr x x where fromChannelRepr = unwrap >>> Just
--instance Monoid a => HasFallback a where fallback = mempty


-- instance ToChannelRepr Unit Unit     where toChannelRepr = accept
-- instance ToChannelRepr Int Int       where toChannelRepr = accept
-- instance ToChannelRepr String String where toChannelRepr = accept
-- -- instance ToChannelRepr (Maybe a) a   where toChannelRepr = map ChannelRepr
-- instance FromChannelRepr Unit Unit     where fromChannelRepr = Just
-- instance FromChannelRepr Int Int       where fromChannelRepr = Just
-- instance FromChannelRepr String String where fromChannelRepr = Just


accept :: forall a. a -> ValueInChannel a
accept = Accepted


decline :: forall a. ValueInChannel a
decline = Declined


empty :: forall a. ValueInChannel a
empty = Empty


{-
ensureTo :: forall repr a. HasFallback repr => ToChannelRepr a repr => a -> ChannelRepr repr
ensureTo = fromMaybe (ChannelRepr fallback) <<< toChannelRepr


ensureFrom :: forall repr a. HasFallback a => FromChannelRepr repr a => ChannelRepr repr -> a
ensureFrom = fromMaybe fallback <<< fromChannelRepr


fallbackByChannelRepr :: forall repr. HasFallback repr => Maybe repr -> ChannelRepr repr
fallbackByChannelRepr = fallbackBy ChannelRepr
-}


{-
class DataToValueInChannelRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class DataToValueInChannelRow rl row repr from to | rl -> row from to, repr -> row from to where
  dataToValueInChannelRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }


class DataFromToValueInChannelRow :: RL.RowList Type -> Row Type -> Type -> Constraint
class DataFromToValueInChannelRow rl row repr | rl -> row, repr -> row
-}


class FromValueInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class FromValueInChannelRowBase rl row repr from to | rl -> row from to, repr -> row from to where
  fromValueInChannelRowBuilder :: Proxy repr -> Proxy rl -> Map String repr -> Builder { | from } { | to }


class ToValueInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Type -> Constraint
class ToValueInChannelRowBase rl row k repr | rl -> row, repr -> row where
  toValueInChannelRowBase :: Proxy repr -> Proxy rl -> (forall field. IsSymbol field => Proxy field -> k) -> Record row -> Map k repr -> Map k repr


class    (RL.RowToList row rl, Record.Keys rl, FromValueInChannelRowBase rl row repr () row) <= FromValueInChannelRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, FromValueInChannelRowBase rl row repr () row) => FromValueInChannelRow rl row repr

class    (RL.RowToList row rl, Record.Keys rl, ToValueInChannelRowBase rl row k repr) <= ToValueInChannelRow rl row k repr
instance (RL.RowToList row rl, Record.Keys rl, ToValueInChannelRowBase rl row k repr) => ToValueInChannelRow rl row k repr


instance fromValueInChannelRowBaseNil :: FromValueInChannelRowBase RL.Nil row repr () () where
  fromValueInChannelRowBuilder _ _ _ = identity
else instance fromValueInChannelRowBaseCons ::
  ( IsSymbol name
  , ToValueInChannel repr a
  , HasFallback repr -- TODO: don't do `fallback`, either store =Maybe= or find another way
  , Row.Cons name (ValueInChannel a) trash row
  , Row.Lacks name from'
  , Row.Cons name (ValueInChannel a) from' to
  , FromValueInChannelRowBase tail row repr from from'
  ) => FromValueInChannelRowBase (RL.Cons name (ValueInChannel a) tail) row repr from to where
  fromValueInChannelRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: ValueInChannel a) =
        case Map.lookup (reflectSymbol nameP) map of
          Just repr -> toValueInChannel repr
          Nothing -> toValueInChannel (fallback :: repr)
      rest = fromValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


instance toChannelReprRowBaseNil :: ToValueInChannelRowBase RL.Nil row k repr where
    toValueInChannelRowBase _ _ _ _ _ = Map.empty
else instance toValueInChannelRowBaseCons ::
  ( Ord k
  , IsSymbol name
  , HasFallback repr -- TODO: don't do `fallback`, either store =Maybe= or find another way
  , FromValueInChannel a repr
  , Row.Cons name (ValueInChannel a) trash row
  , ToValueInChannelRowBase tail row k repr
  ) => ToValueInChannelRowBase (RL.Cons name (ValueInChannel a) tail) row k repr where
    toValueInChannelRowBase prepr _ toKey rec prev =
      Map.insert (toKey nameP) value rest
      where
        nameP = Proxy :: _ name
        value = _ensureTo $ R.get nameP rec
        rest = toValueInChannelRowBase prepr (Proxy :: _ tail) toKey rec prev


_ensureTo :: forall a repr. HasFallback repr => FromValueInChannel a repr => ValueInChannel a -> repr
_ensureTo = case _ of
  Accepted a -> fromMaybe fallback $ fromValueInChannel a
  Declined -> fallback
  Empty -> fallback


{-
instance dataFromToValueInChannelRowNil :: DataFromToValueInChannelRow RL.Nil row repr
else instance dataFromToValueInChannelRowCons ::
  ( IsSymbol name
  , ToChannelRepr a repr
  , FromChannelRepr repr a
  , Row.Cons name a trash row
  , DataFromToValueInChannelRow tail row repr
  -- , Row.Lacks name from'
  -- , Row.Cons name (Maybe (ChannelRepr repr)) from' to
  ) => DataFromToValueInChannelRow (RL.Cons name a tail) row repr


instance dataToValueInChannelRowNil :: DataToValueInChannelRow RL.Nil row repr () () where
  dataToChannelReprRowBuilder _ _ _ = identity
else instance dataToValueInChannelRowCons ::
  ( IsSymbol name
  , ToChannelRepr a repr
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name (Maybe (ChannelRepr repr)) from' to
  , DataToValueInChannelRow tail row repr from from'
  ) => DataToValueInChannelRow (RL.Cons name a tail) row repr from to where
  dataToValueInChannelRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = toChannelRepr $ R.get nameP rec
      rest = dataToValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


dataToValueInChannelRow :: forall row rl repr row'
   . RL.RowToList row rl
  => DataToValueInChannelRow rl row repr () row'
  => Record row
  -> Record row'
dataToValueInChannelRow r = Builder.build builder {}
  where
    builder = dataToValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) r
  -}


fromValueInChannelRow :: forall row rl repr
   . FromValueInChannelRow rl row repr
  => Map String repr
  -> Record row
fromValueInChannelRow map = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = fromValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) map


fromMap :: forall row rl repr
   . FromValueInChannelRow rl row repr
  => Map String repr
  -> Record row
fromMap = fromValueInChannelRow


toMap :: forall k rl row repr
    .  ToValueInChannelRow rl row k repr
  => (forall s. IsSymbol s => Proxy s -> k)
  -> Record row
  -> Map k repr
toMap toKey record = toValueInChannelRowBase (Proxy :: _ repr) (Proxy :: _ rl) toKey record Map.empty


{- TODO
inbetween :: forall a b reprA reprB. HasFallback reprB => FromChannelRepr reprA a => ToChannelRepr b reprB => (a -> b) -> (reprA -> reprB)
inbetween f reprA = fromMaybe fallback $ unwrap <$> (toChannelRepr =<< f <$> (fromChannelRepr $ ChannelRepr reprA))


inbetween' :: forall a reprA reprB. HasFallback reprB => FromChannelRepr reprA a => ToChannelRepr a reprB => Proxy a -> (reprA -> reprB)
inbetween' _ = inbetween (identity :: a -> a)
-}