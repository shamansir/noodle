-- from: https://github.com/rubenpieters/purescript-subrecord

module Data.SubRecord.Internal where

-- | A `SubRecord x` is a record which may contain values for labels in x
-- | as opposed to a `Record x` which must contain values for labels in x
foreign import data SubRecord :: # Type -> Type