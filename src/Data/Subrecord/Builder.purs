-- from: https://github.com/rubenpieters/purescript-subrecord

module Data.SubRecord.Builder
  ( Builder
  , build
  , insert
  , modify
  , delete
  , rename
  , merge
  ) where

import Prelude

import Data.SubRecord.Internal (SubRecord)

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class Lacks, class Cons, class Union)

import Unsafe.Coerce (unsafeCoerce)

foreign import copyRecord :: forall r1. SubRecord r1 -> SubRecord r1
foreign import unsafeInsert :: forall a r1 r2. String -> a -> SubRecord r1 -> SubRecord r2
foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> SubRecord r1 -> SubRecord r2
foreign import unsafeDelete :: forall r1 r2. String -> SubRecord r1 -> SubRecord r2
foreign import unsafeRename :: forall r1 r2. String -> String -> SubRecord r1 -> SubRecord r2
foreign import unsafeMerge :: forall r1 r2 r3. SubRecord r1 -> SubRecord r2 -> SubRecord r3

-- | A `Builder` can be used to `build` a subrecord by incrementally adding
-- | fields in-place, instead of using `insert` and repeatedly generating new
-- | immutable subrecords which need to be garbage collected.
-- |
-- | The `Category` instance for `Builder` can be used to compose builders.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (insert x (Just 42) >>> insert y (Just "testing")) (mkSubRecord {}) :: SubRecord ( x :: Int, y :: String )
-- | ```
newtype Builder a b = Builder (a -> b)

-- | Build a record, starting from some other record.
build :: forall r1 r2. Builder (SubRecord r1) (SubRecord r2) -> SubRecord r1 -> SubRecord r2
build (Builder b) r1 = b (copyRecord r1)

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
derive newtype instance categoryBuilder :: Category Builder

-- | Build by inserting a new field if the value is `Just x`.
-- | The subrecord is unchanged if the value is `Nothing`.
insert
  :: forall l a r1 r2
   . Cons l a r1 r2
  => Lacks l r1
  => IsSymbol l
  => SProxy l
  -> Maybe a
  -> Builder (SubRecord r1) (SubRecord r2)
insert l (Just a) = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1
insert l Nothing = Builder \r1 -> unsafeCoerce r1

-- | Build by modifying a potentially existing field.
modify
  :: forall l a b r r1 r2
   . Cons l a r r1
  => Cons l b r r2
  => IsSymbol l
  => SProxy l
  -> (a -> b)
  -> Builder (SubRecord r1) (SubRecord r2)
modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1

-- | Build by deleting a potentially existing field.
delete
  :: forall l a r1 r2
   . IsSymbol l
   => Lacks l r1
   => Cons l a r1 r2
   => SProxy l
   -> Builder (SubRecord r2) (SubRecord r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

-- | Build by renaming a potentially existing field.
rename :: forall l1 l2 a r1 r2 r3
   . IsSymbol l1
  => IsSymbol l2
  => Cons l1 a r2 r1
  => Lacks l1 r2
  => Cons l2 a r2 r3
  => Lacks l2 r2
  => SProxy l1
  -> SProxy l2
  -> Builder (SubRecord r1) (SubRecord r3)
rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1

-- | Build by merging fields from another subrecord.
merge
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => SubRecord r2
  -> Builder (SubRecord r1) (SubRecord r3)
merge r2 = Builder \r1 -> unsafeMerge r1 r2