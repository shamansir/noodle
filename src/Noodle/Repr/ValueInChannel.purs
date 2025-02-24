module Noodle.Repr.ValueInChannel
    ( ValueInChannel
    , accept, decline, empty, _missingKey, _reportMissingKey, toFallback, _backToValue
    , resolve
    , class ToValueInChannel, toValueInChannel
    , class FromValueInChannel, fromValueInChannel
    , class FromToValueInChannel
    -- , exists, wrap, unwrap
    , class ToValuesInChannelRow, class ToValuesInChannelRowBase, toValuesInChannelRowBuilder
    , class FromValuesInChannelRow, class FromValuesInChannelRowBase, fromValuesInChannelRowBase
    , class ValuesToReprRow, valuesToReprRowBuilder
    , LiftMethod, AcceptAll, DeclineAll, class LiftAllValuesRow, liftAllValuesRowBuilder
    , acceptAll, declineAll
    -- , class DataFromFromValuesInChannelRow
    , fromMap, toMap, toMaybe
    , recordWithValuesToRepr
    , inbetween, inbetween', inbetweenB
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
import Data.Tuple.Nested ((/\), type (/\))

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
import Record.Extra (class Keys) as Record
import Record.Builder (Builder)
import Record.Builder as Builder

import Noodle.Repr.HasFallback (class HasFallback, fallback, fallbackBy)


data ValueInChannel a
  = Accepted a
  | Declined -- { declined :: b, current :: a }
  | MissingKey String
  | Empty


derive instance Eq a => Eq (ValueInChannel a)
derive instance Functor ValueInChannel


-- instance HasFallback (ValueInChannel a) where fallback = Empty


class ToValueInChannel repr a where
    toValueInChannel :: repr -> ValueInChannel a


class FromValueInChannel a repr where
    fromValueInChannel :: a -> repr


instance Apply ValueInChannel where -- TODO: should be ensured to align with `Apply` laws
  apply :: forall a b. ValueInChannel (a -> b) -> ValueInChannel a -> ValueInChannel b
  apply = _apply


instance Bind ValueInChannel where -- TODO: should be ensured to align with `Bind` laws
  bind :: forall a b. ValueInChannel a -> (a -> ValueInChannel b) -> ValueInChannel b
  bind = flip _bind


instance Show a => Show (ValueInChannel a) where
    show (Accepted r) = "✓ " <> show r
    show Declined = "𐄂"
    show (MissingKey key) = "?:" <> key
    show Empty = "∅"


class    (FromValueInChannel a repr, ToValueInChannel repr a) <= FromToValueInChannel a repr
instance (FromValueInChannel a repr, ToValueInChannel repr a) => FromToValueInChannel a repr


accept :: forall a. a -> ValueInChannel a
accept = Accepted


decline :: forall a. ValueInChannel a
decline = Declined


empty :: forall a. ValueInChannel a
empty = Empty


_missingKey :: forall a. String -> ValueInChannel a
_missingKey = MissingKey


 -- TODO: should be ensured to align with `Apply` laws
_apply :: forall a b. ValueInChannel (a -> b) -> ValueInChannel a -> ValueInChannel b
_apply vicf = case _ of
    Accepted a ->
      case vicf of
        Accepted f -> Accepted $ f a
        Declined -> Declined
        MissingKey key -> MissingKey key
        Empty -> Empty
    Declined -> Declined
    MissingKey key -> MissingKey key
    Empty -> Empty


 -- TODO: should be ensured to align with `Bind` and `Apply` laws
_bind :: forall a b. (a -> ValueInChannel b) -> ValueInChannel a -> ValueInChannel b
_bind f = case _ of
  Accepted a -> f a
  Declined -> Declined
  MissingKey key -> MissingKey key
  Empty -> Empty


_backToValue :: forall a repr. ToValueInChannel repr a => ValueInChannel repr -> ValueInChannel a
_backToValue = _bind toValueInChannel


_reportMissingKey :: forall a. String -> Maybe (ValueInChannel a) -> ValueInChannel a
_reportMissingKey key =
  case _ of
    Just vicVal -> vicVal
    Nothing -> _missingKey key


toFallback :: forall a. HasFallback a => ValueInChannel a -> a
toFallback = toMaybe >>> fromMaybe fallback


toMaybe :: forall a. ValueInChannel a -> Maybe a
toMaybe =
    case _ of
        Accepted a -> Just a
        _ -> Nothing


data LiftMethod


foreign import data AcceptAll  :: LiftMethod
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


{-
class ReprToValueRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ReprToValueRow rl row repr from to | rl -> row from to, repr -> row from to where
  reprToValueRowBuilder :: Proxy repr -> Proxy rl -> Record row -> Builder { | from } { | to }
-}


class FromValuesInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Type -> Constraint
class FromValuesInChannelRowBase rl row k repr | rl -> row, repr -> row where
  fromValuesInChannelRowBase :: Proxy repr -> Proxy rl -> (forall field. IsSymbol field => Proxy field -> k) -> Record row -> Map k (ValueInChannel repr) -> Map k (ValueInChannel repr)


class ToValuesInChannelRowBase :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ToValuesInChannelRowBase rl row repr from to | rl -> row from to, repr -> row from to where
  toValuesInChannelRowBuilder :: Proxy repr -> Proxy rl -> Map String (ValueInChannel repr) -> Builder { | from } { | to }


class    (RL.RowToList row rl, Record.Keys rl, FromValuesInChannelRowBase rl row k repr) <= FromValuesInChannelRow rl row k repr
instance (RL.RowToList row rl, Record.Keys rl, FromValuesInChannelRowBase rl row k repr) => FromValuesInChannelRow rl row k repr


class    (RL.RowToList row rl, Record.Keys rl, ToValuesInChannelRowBase rl row repr () row) <= ToValuesInChannelRow rl row repr
instance (RL.RowToList row rl, Record.Keys rl, ToValuesInChannelRowBase rl row repr () row) => ToValuesInChannelRow rl row repr


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


instance fromChannelReprRowBaseNil :: FromValuesInChannelRowBase RL.Nil row k repr where
    fromValuesInChannelRowBase _ _ _ _ _ = Map.empty
else instance fromValuesInChannelRowBaseCons ::
  ( Ord k
  , IsSymbol name
  , FromValueInChannel a repr
  , Row.Cons name a trash row
  , FromValuesInChannelRowBase tail row k repr
  ) => FromValuesInChannelRowBase (RL.Cons name a tail) row k repr where
    fromValuesInChannelRowBase prepr _ toKey rec prev =
      Map.insert (toKey nameP) value rest
      where
        nameP = Proxy :: _ name
        value = Accepted $ fromValueInChannel $ R.get nameP rec
        rest = fromValuesInChannelRowBase prepr (Proxy :: _ tail) toKey rec prev


instance toValuesInChannelRowBaseNil :: ToValuesInChannelRowBase RL.Nil row repr () () where
  toValuesInChannelRowBuilder _ _ _ = identity
else instance toValuesInChannelRowBaseCons ::
  ( IsSymbol name
  , HasFallback a
  , ToValueInChannel repr a
  , Row.Cons name a trash row
  , Row.Lacks name from'
  , Row.Cons name a from' to
  , ToValuesInChannelRowBase tail row repr from from'
  ) => ToValuesInChannelRowBase (RL.Cons name a tail) row repr from to where
  toValuesInChannelRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: ValueInChannel a) =
        case Map.lookup (reflectSymbol nameP) map of
          Just repr -> _backToValue repr
          Nothing -> MissingKey $ reflectSymbol nameP
      rest = toValuesInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP $ toFallback val


fromMap :: forall row rl repr
   . ToValuesInChannelRow rl row repr
  => Map String (ValueInChannel repr)
  -> Record row
fromMap theMap = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = toValuesInChannelRowBuilder (Proxy :: _ repr) (Proxy :: _ rl) theMap


toMap :: forall k rl row repr
   . FromValuesInChannelRow rl row k repr
  => (forall s. IsSymbol s => Proxy s -> k)
  -> Record row
  -> Map k (ValueInChannel repr)
toMap toKey record = fromValuesInChannelRowBase (Proxy :: _ repr) (Proxy :: _ rl) toKey record Map.empty


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



inbetween :: forall a b reprA reprB. ToValueInChannel reprA a => FromValueInChannel b reprB => (a -> b) -> (reprA -> ValueInChannel reprB)
inbetween f reprA = fromValueInChannel <$> f <$> (toValueInChannel reprA :: ValueInChannel a)


inbetween' :: forall a reprA reprB. ToValueInChannel reprA a => FromValueInChannel a reprB => Proxy a -> (reprA -> ValueInChannel reprB)
inbetween' _ = inbetween (identity :: a -> a)


inbetweenB :: forall a b reprA reprB. ToValueInChannel reprA a => FromValueInChannel b reprB => (a -> ValueInChannel b) -> (reprA -> ValueInChannel reprB)
inbetweenB f reprA = fromValueInChannel <$> (f =<< (toValueInChannel reprA :: ValueInChannel a))


type Resolve a b =
    { accept :: a -> b
    , decline :: b
    , missingKey :: String -> b
    , empty :: b
    }


resolve :: forall a b. Resolve a b -> ValueInChannel a -> b
resolve rec = case _ of
    Accepted a -> rec.accept a
    Declined -> rec.decline
    MissingKey key -> rec.missingKey key
    Empty -> rec.empty