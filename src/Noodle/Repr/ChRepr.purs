module Noodle.Repr.ChRepr
    ( ValueInChannel
    , accept, decline, empty
    , class ToValueInChannel, toValueInChannel
    , class FromValueInChannel, fromValueInChannel
    , class FromToValueInChannel
    -- , exists, wrap, unwrap
    -- , class DataFromValueInChannelRow, dataFromValueInChannelRow, dataFromValueInChannelRowBuilder
    , class ToValueInChannelRow, class ToValueInChannelRowBase, toValueInChannelRowBuilder
    , class FromValueInChannelRow, class FromValueInChannelRowBase, fromValueInChannelRowBase
    , class ValuesToReprRow, valuesToReprRowBuilder
    , LiftMethod, AcceptAll, DeclineAll, class LiftAllValuesRow, liftAllValuesRowBuilder
    , acceptAll, declineAll
    -- , class DataFromFromValueInChannelRow
    , class ReadChannelRepr, readChannelRepr
    , class WriteChannelRepr, writeChannelRepr
    , class ReadWriteChannelRepr
    , fromMap, toMap
    , recordWithValuesToRepr
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
  | MissingKey String
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
    fromValueInChannel :: a -> repr


class ReadChannelRepr repr where
    readChannelRepr :: String -> Maybe repr


class WriteChannelRepr repr where
    writeChannelRepr :: repr -> String


instance Show a => Show (ValueInChannel a) where
    show (Accepted r) = "✓ " <> show r
    show Declined = "𐄂"
    show (MissingKey key) = "?:" <> key
    show Empty = "∅"


class    (ReadChannelRepr repr, WriteChannelRepr repr) <= ReadWriteChannelRepr repr
instance (ReadChannelRepr repr, WriteChannelRepr repr) => ReadWriteChannelRepr repr


class    (FromValueInChannel repr a, ToValueInChannel a repr) <= FromToValueInChannel a repr
instance (FromValueInChannel repr a, ToValueInChannel a repr) => FromToValueInChannel a repr


accept :: forall a. a -> ValueInChannel a
accept = Accepted


decline :: forall a. ValueInChannel a
decline = Declined


empty :: forall a. ValueInChannel a
empty = Empty


data LiftMethod


foreign import data AcceptAll :: LiftMethod
foreign import data DeclineAll :: LiftMethod


class Lifter :: LiftMethod -> Type -> Constraint
class Lifter method a where
  liftValue :: Proxy method -> a -> ValueInChannel a


instance Lifter AcceptAll a where
  liftValue = const accept


instance Lifter DeclineAll a where
  liftValue = const $ const decline


class LiftAllValuesRow :: LiftMethod -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class LiftAllValuesRow method rl row from to | rl -> row from to, method -> row from to where
  liftAllValuesRowBuilder :: Proxy method -> Proxy rl -> Record row -> Builder { | from } { | to }


class ValuesToReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ValuesToReprRow rl row repr from to | rl -> row from to, repr -> row from to where
  valuesToReprRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }


class ReprToValueRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ReprToValueRow rl row repr from to | rl -> row from to, repr -> row from to where
  reprToValueRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }


class FromValueInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Type -> Constraint
class FromValueInChannelRowBase rl row k repr | rl -> row, repr -> row where
  fromValueInChannelRowBase :: Proxy repr -> Proxy rl -> (forall field. IsSymbol field => Proxy field -> k) -> Record row -> Map k (ValueInChannel repr) -> Map k (ValueInChannel repr)


class ToValueInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ToValueInChannelRowBase rl row repr from to | rl -> row from to, repr -> row from to where
  toValueInChannelRowBuilder :: Proxy repr -> Proxy rl -> Map String repr -> Builder { | from } { | to }


class    (RL.RowToList row rl, Record.Keys rl, FromValueInChannelRowBase rl row k repr) <= FromValueInChannelRow rl row k repr
instance (RL.RowToList row rl, Record.Keys rl, FromValueInChannelRowBase rl row k repr) => FromValueInChannelRow rl row k repr


class    (RL.RowToList row rl, Record.Keys rl, ToValueInChannelRowBase rl row repr () row) <= ToValueInChannelRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, ToValueInChannelRowBase rl row repr () row) => ToValueInChannelRow rl row repr


instance valueToReprRowNil :: ValuesToReprRow RL.Nil row repr () () where
  valuesToReprRowBuilder _ _ _ = identity
else instance valueToReprRowCons ::
  ( IsSymbol name
  , FromValueInChannel a repr
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name repr from' to
  , ValuesToReprRow tail row repr from from'
  ) => ValuesToReprRow (RL.Cons name a tail) row repr from to where
  valuesToReprRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = fromValueInChannel $ R.get nameP rec
      rest = valuesToReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


instance liftAllValuesRowNil :: LiftAllValuesRow method RL.Nil row () () where
  liftAllValuesRowBuilder _ _ _ = identity
else instance liftAllValuesRowNilCons ::
  ( IsSymbol name
  , Lifter method a
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name (ValueInChannel a) from' to
  , LiftAllValuesRow method tail row from from'
  ) => LiftAllValuesRow method (RL.Cons name a tail) row from to where
  liftAllValuesRowBuilder pmethod _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = liftValue pmethod $ R.get nameP rec
      rest = liftAllValuesRowBuilder (Proxy :: _ method) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


instance fromChannelReprRowBaseNil :: FromValueInChannelRowBase RL.Nil row k repr where
    fromValueInChannelRowBase _ _ _ _ _ = Map.empty
else instance fromValueInChannelRowBaseCons ::
  ( Ord k
  , IsSymbol name
  , FromValueInChannel a repr
  , Row.Cons name (ValueInChannel a) trash row
  , FromValueInChannelRowBase tail row k repr
  ) => FromValueInChannelRowBase (RL.Cons name (ValueInChannel a) tail) row k repr where
    fromValueInChannelRowBase prepr _ toKey rec prev =
      Map.insert (toKey nameP) value rest
      where
        nameP = Proxy :: _ name
        value = fromValueInChannel <$> R.get nameP rec
        rest = fromValueInChannelRowBase prepr (Proxy :: _ tail) toKey rec prev


instance toValueInChannelRowBaseNil :: ToValueInChannelRowBase RL.Nil row repr () () where
  toValueInChannelRowBuilder _ _ _ = identity
else instance toValueInChannelRowBaseCons ::
  ( IsSymbol name
  , ToValueInChannel repr a
  , Row.Cons name (ValueInChannel a) trash row
  , Row.Lacks name from'
  , Row.Cons name (ValueInChannel a) from' to
  , ToValueInChannelRowBase tail row repr from from'
  ) => ToValueInChannelRowBase (RL.Cons name (ValueInChannel a) tail) row repr from to where
  toValueInChannelRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: ValueInChannel a) =
        case Map.lookup (reflectSymbol nameP) map of
          Just repr -> toValueInChannel repr
          Nothing -> MissingKey $ reflectSymbol nameP
      rest = toValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


fromMap :: forall row rl repr
   . ToValueInChannelRow rl row repr
  => Map String repr
  -> Record row
fromMap map = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = toValueInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) map


toMap :: forall k rl row repr
   . FromValueInChannelRow rl row k repr
  => (forall s. IsSymbol s => Proxy s -> k)
  -> Record row
  -> Map k (ValueInChannel repr)
toMap toKey record = fromValueInChannelRowBase (Proxy :: _ repr) (Proxy :: _ rl) toKey record Map.empty


recordWithValuesToRepr :: forall row rl repr row'
   . RL.RowToList row rl
  => ValuesToReprRow rl row repr () row'
  => Record row
  -> Record row'
recordWithValuesToRepr r = Builder.build builder {}
  where
    builder = valuesToReprRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) r


acceptAll :: forall row rl row'
   . RL.RowToList row rl
  => LiftAllValuesRow AcceptAll rl row () row'
  => Record row
  -> Record row'
acceptAll r = Builder.build builder {}
  where
    builder = liftAllValuesRowBuilder (Proxy :: _ AcceptAll) (Proxy :: _ rl) r


declineAll :: forall row rl row'
   . RL.RowToList row rl
  => LiftAllValuesRow DeclineAll rl row () row'
  => Record row
  -> Record row'
declineAll r = Builder.build builder {}
  where
    builder = liftAllValuesRowBuilder (Proxy :: _ DeclineAll) (Proxy :: _ rl) r


{- TODO
inbetween :: forall a b reprA reprB. HasFallback reprB => FromChannelRepr reprA a => ToChannelRepr b reprB => (a -> b) -> (reprA -> reprB)
inbetween f reprA = fromMaybe fallback $ unwrap <$> (toChannelRepr =<< f <$> (fromChannelRepr $ ChannelRepr reprA))


inbetween' :: forall a reprA reprB. HasFallback reprB => FromChannelRepr reprA a => ToChannelRepr a reprB => Proxy a -> (reprA -> reprB)
inbetween' _ = inbetween (identity :: a -> a)
-}