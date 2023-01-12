module Data.SubRecord (module Data.SubRecord, module Exported) where

-- from: https://github.com/rubenpieters/purescript-subrecord

import Data.SubRecord.Internal (SubRecord) as Exported

import Data.SubRecord.Internal
import Data.Maybe (Maybe)
import Record.Builder as Record
import Data.SubRecord.Unsafe
import Data.Symbol
import Type.Row (class Lacks, class Cons, class Union, class Nub)

import Unsafe.Coerce (unsafeCoerce)

mkSubRecord :: forall a x r.
               Union a x r =>
               Record a -> SubRecord r
mkSubRecord = unsafeCoerce

foreign import passNullContext :: forall a b. a -> b

unSubRecord :: forall x r.
               (forall a.
                Union a x r =>
                Record a -> Record r
               ) ->
               SubRecord r -> Record r
unSubRecord = passNullContext

-- signature commented, because it doesn't seem to compile if explicitly annotated
--withDefaults :: forall a. Record a -> SubRecord a -> Record a
--withDefaults ∷ ∀ a2 (t14 ∷ Row Type) (t17 ∷ Row Type) (t19 ∷ Row Type). Union t17 a2 t19 ⇒ Nub t19 t14 ⇒ Record t17 → SubRecord t14 → Record t14
withDefaults defaults = unSubRecord (\r -> Record.build (Record.merge defaults) r)

-- | Get a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | get (SProxy :: SProxy "x") :: forall r a. SubRecord ( x :: a | r ) -> Maybe a
-- | ```
get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => SProxy l
  -> SubRecord r
  -> Maybe a
get l r = unsafeGet (reflectSymbol l) r