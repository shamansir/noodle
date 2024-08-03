
{- From: https://github.com/justinwoo/purescript-record-extra -}

module Record.Extra where

import Prelude

import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List, (:))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as R
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

mapRecord :: forall row xs a b row'
   . RL.RowToList row xs
  => MapRecord xs row a b () row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord f r = Builder.build builder {}
  where
    builder = mapRecordBuilder (Proxy :: _ xs) f r

class MapRecord (xs :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | xs -> row a b from to where
  mapRecordBuilder :: Proxy xs -> (a -> b) -> Record row -> Builder { | from } { | to }

instance mapRecordCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapRecord tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) => MapRecord (RL.Cons name a tail) row a b from to where
  mapRecordBuilder _ f r =
    first <<< rest
    where
      nameP = Proxy :: _ name
      val = f $ R.get nameP r
      rest = mapRecordBuilder (Proxy :: _ tail) f r
      first = Builder.insert nameP val

instance mapRecordNil :: MapRecord RL.Nil row a b () () where
  mapRecordBuilder _ _ _ = identity

class ZipRecord
  ( rla :: RL.RowList Type)
  ( ra :: Row Type )
  ( rlb :: RL.RowList Type)
  ( rb :: Row Type )
  ( from :: Row Type )
  ( to :: Row Type )
  | rla -> ra from to
  , rlb -> rb from to
  where
    zipRecordImpl ::
         Proxy rla
      -> Record ra
      -> Proxy rlb
      -> Record rb
      -> Builder { | from } { | to }

instance zipRecordNil :: ZipRecord RL.Nil trashA RL.Nil trashB () ()
  where
    zipRecordImpl _ _ _ _ = identity

instance zipRecordCons
    :: ( IsSymbol k
       , Row.Cons k a trashA ra
       , Row.Cons k b trashB rb
       , Row.Cons k (Tuple a b) from' to
       , Row.Lacks k from'
       , ZipRecord ta ra tb rb from from'
       )
    => ZipRecord
         (RL.Cons k a ta)
         ra
         (RL.Cons k b tb)
         rb
         from
         to
  where
    zipRecordImpl _ ra _ rb = first <<< tail
      where
        name = Proxy :: _ k
        head = Tuple (R.get name ra) (R.get name rb)
        ta = Proxy :: _ ta
        tb = Proxy :: _ tb
        tail = zipRecordImpl ta ra tb rb
        first = Builder.insert name head

zipRecord :: forall ta ra tb rb rc
   . RL.RowToList ra ta
  => RL.RowToList rb tb
  => ZipRecord ta ra tb rb () rc
  => Record ra
  -> Record rb
  -> Record rc
zipRecord ra rb = Builder.build builder {}
  where
    ta = Proxy :: _ ta
    tb = Proxy :: _ tb
    builder = zipRecordImpl ta ra tb rb

class Keys (xs :: RL.RowList Type) where
  keysImpl :: Proxy xs -> List String

instance nilKeys :: Keys RL.Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (Proxy :: _ name)
      rest = keysImpl (Proxy :: _ tail)

keys :: forall g row rl
   . RL.RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (Proxy :: _ rl)

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick :: forall a r b l.
     Row.Union b r a
  => RL.RowToList b l
  => Keys l
  => Record a
  -> Record b
pick = runFn2 pickFn ks
  where
    ks = fromFoldable $ keys (Proxy :: _ b)

slistKeys :: forall g tuples rl
   . SListToRowList tuples rl
  => Keys rl
  => g tuples
  -> List String
slistKeys _ = keysImpl (Proxy :: _ rl)

data SList
foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

infixr 6 type SCons as :::

class SListToRowList (xs :: SList) (rl :: RL.RowList Type) | xs -> rl, rl -> xs

instance slToRlSNil :: SListToRowList SNil RL.Nil

instance slToRlSCons ::
  ( SListToRowList sTail tail
  ) => SListToRowList (SCons name sTail) (RL.Cons name trash tail)

class OrdRecord (rl :: RL.RowList Type) row
  | rl -> row
  where
    compareRecordImpl :: Proxy rl -> Record row -> Record row -> Ordering

instance ordRecordCons ::
  ( IsSymbol name
  , Ord ty
  , Row.Cons name ty trash row
  , OrdRecord tail row
  ) => OrdRecord (RL.Cons name ty tail) row where
  compareRecordImpl _ a b =
    case compare valA valB of
         EQ -> compareRecordImpl tailp a b
         ordering -> ordering
    where
      namep = Proxy :: _ name
      valA = R.get namep a
      valB = R.get namep b
      tailp = Proxy :: _ tail

instance ordRecordNil :: OrdRecord RL.Nil row where
  compareRecordImpl _ _ _ = EQ

compareRecord :: forall row rl
   . RL.RowToList row rl
  => OrdRecord rl row
  => Record row
  -> Record row
  -> Ordering
compareRecord a b = compareRecordImpl (Proxy :: _ rl) a b

class Functor m <= SequenceRecord (rl :: RL.RowList Type) row from to m
  | rl -> row from to m
  where
    sequenceRecordImpl :: Proxy rl -> Record row -> m (Builder { | from } { | to })

instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Functor m
  , Row.Lacks name ()
  , Row.Cons name ty () to
  ) => SequenceRecord (RL.Cons name (m ty) RL.Nil) row () to m where
  sequenceRecordImpl _ a  =
       Builder.insert namep <$> valA
    where
      namep = Proxy :: _ name
      valA = R.get namep a

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Apply m
  , SequenceRecord tail row from from' m
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => SequenceRecord (RL.Cons name (m ty) tail) row from to m where
  sequenceRecordImpl _ a  =
       fn <$> valA <*> rest
    where
      namep = Proxy :: _ name
      valA = R.get namep a
      tailp = Proxy :: _ tail
      rest = sequenceRecordImpl tailp a
      fn valA' rest' = Builder.insert namep valA' <<< rest'

instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row () () m where
  sequenceRecordImpl _ _ = pure identity

sequenceRecord :: forall row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row () row' m
  => Record row
  -> m (Record row')
sequenceRecord a = Builder.build <@> {} <$> builder
  where
    builder = sequenceRecordImpl (Proxy :: _ rl) a