module Data.Repr
    ( Repr(..)
    , ensureTo, ensureFrom
    , class HasFallback, fallback, fallbackByRepr, fallbackBy
    , class ToRepr, toRepr
    , class FromRepr, fromRepr
    , exists, wrap, unwrap
    , class ToReprRow, toReprRow, toReprRowBuilder
    , class FromReprRow, fromReprRow, fromReprRowBuilder
    , class FromToReprRow
    , class ReadRepr, readRepr
    , class WriteRepr, writeRepr
    , class ReadWriteRepr
    , fromMap
    )
    where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.SProxy (reflect')

import Prim.Row as Row
import Prim.RowList as RL

import Record (get) as R
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


-- instance HasFallback x => FromRepr x x where fromRepr = unwrap >>> Just
-- instance HasFallback x => ToRepr x x where toRepr = wrap >>> Just
--instance Monoid a => HasFallback a where fallback = mempty


instance HasFallback Unit where fallback = unit
instance HasFallback Int where fallback = 0
instance HasFallback String where fallback = ""


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


class ToReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class ToReprRow xs row repr from to | xs -> row from to, repr -> row from to where
  toReprRowBuilder :: Proxy repr -> Proxy xs -> Record row -> Builder { | from } { | to }


class FromReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class FromReprRow xs row repr from to | xs -> row from to, repr -> row from to where
  fromReprRowBuilder :: Proxy repr -> Proxy xs -> Map String (Repr repr) -> Builder { | from } { | to }


class FromToReprRow :: RL.RowList Type -> Row Type -> Type -> Constraint
class FromToReprRow xs row repr | xs -> row, repr -> row


instance fromReprRowNil :: FromReprRow RL.Nil row repr () () where
  fromReprRowBuilder _ _ _ = identity
else instance fromReprRowCons ::
  ( IsSymbol name
  , HasFallback a
  , FromRepr repr a
  , Row.Cons name a trash row
  -- , ToReprRow tail row repr from from'
  , Row.Lacks name from'
  , Row.Cons name a from' to
  , FromReprRow tail row repr from from'
  ) => FromReprRow (RL.Cons name a tail) row repr from to where
  fromReprRowBuilder _ _ map =
    first <<< rest
    where
      nameP = Proxy :: _ name
      (val :: a) = fromMaybe fallback $ fromRepr =<< Map.lookup (reflectSymbol nameP) map
      rest = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) map
      first = Builder.insert nameP val


instance fromToReprRowNil :: FromToReprRow RL.Nil row repr
else instance fromToReprRowCons ::
  ( IsSymbol name
  , ToRepr a repr
  , FromRepr repr a
  , Row.Cons name a trash row
  , FromToReprRow tail row repr
  -- , Row.Lacks name from'
  -- , Row.Cons name (Maybe (Repr repr)) from' to
  ) => FromToReprRow (RL.Cons name a tail) row repr


instance toReprRowNil :: ToReprRow RL.Nil row repr () () where
  toReprRowBuilder _ _ _ = identity
else instance toReprRowCons ::
  ( IsSymbol name
  , ToRepr a repr
  , Row.Cons name a trash row
  , ToReprRow tail row repr from from'
  , Row.Lacks name from'
  , Row.Cons name (Maybe (Repr repr)) from' to
  ) => ToReprRow (RL.Cons name a tail) row repr from to where
  toReprRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = toRepr $ R.get nameP rec
      rest = toReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


toReprRow :: forall row xs repr row'
   . RL.RowToList row xs
  => ToReprRow xs row repr () row'
  => Record row
  -> Record row'
toReprRow r = Builder.build builder {}
  where
    builder = toReprRowBuilder (Proxy :: _ repr) (Proxy :: _ xs) r


fromReprRow :: forall row xs repr
   . RL.RowToList row xs
  => FromReprRow xs row repr () row
  => Map String (Repr repr) -> Record row
fromReprRow map = Builder.build builder {}
  where
    builder :: Builder (Record ()) (Record row)
    builder = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ xs) map


fromMap :: forall row xs repr
   . RL.RowToList row xs
  => FromReprRow xs row repr () row
  => Map String (Repr repr) -> Record row
fromMap = fromReprRow


{-
class FromReprRow :: RL.RowList Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class FromReprRow xs row repr from to | xs -> row from to, repr -> row from to where
  fromReprRowBuilder :: Proxy repr -> Proxy xs -> Record row -> Builder { | from } { | to }


instance fromReprRowNil :: FromReprRow RL.Nil row repr () () where
  fromReprRowBuilder _ _ _ = identity

else instance fromReprRowCons ::
  ( IsSymbol name
  , FromRepr repr a
  , Row.Cons name (Maybe (Repr repr)) trash row
  , FromReprRow tail row repr from from'
  , Row.Lacks name from'
  , Row.Cons name (Maybe a) from' to
  ) => FromReprRow (RL.Cons name (Maybe (Repr repr)) tail) row repr from to where
  fromReprRowBuilder _ _ rec =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = fromRepr =<< R.get nameP rec
      rest = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ tail) rec
      first = Builder.insert nameP val


fromReprRow :: forall row xs repr row'
   . RL.RowToList row xs
  => FromReprRow xs row repr () row'
  => Record row
  -> Record row'
fromReprRow r = Builder.build builder {}
  where
    builder = fromReprRowBuilder (Proxy :: _ repr) (Proxy :: _ xs) r
-}