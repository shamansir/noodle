-- from: https://github.com/rubenpieters/purescript-subrecord

module Data.SubRecord.Unsafe
  ( unsafeGetFn
  , unsafeSetFn
  , unsafeDeleteFn
  , unsafeHasFn
  , unsafeGet
  , unsafeSet
  , unsafeDelete
  , unsafeHas
  ) where

import Data.SubRecord.Internal (SubRecord)

import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)

foreign import unsafeGetFn :: forall r a. Fn4 (a -> Maybe a) (Maybe a) String (SubRecord r) (Maybe a)
foreign import unsafeSetFn :: forall r1 r2 a. Fn3 String a (SubRecord r1) (SubRecord r2)
foreign import unsafeDeleteFn :: forall r1 r2. Fn2 String (SubRecord r1) (SubRecord r2)
foreign import unsafeHasFn :: forall r1. Fn2 String (SubRecord r1) Boolean

unsafeGet :: forall r a. String -> SubRecord r -> Maybe a
unsafeGet = runFn4 unsafeGetFn Just Nothing

unsafeSet :: forall r1 r2 a. String -> a -> SubRecord r1 -> SubRecord r2
unsafeSet = runFn3 unsafeSetFn

unsafeDelete :: forall r1 r2. String -> SubRecord r1 -> SubRecord r2
unsafeDelete = runFn2 unsafeDeleteFn

unsafeHas :: forall r1. String -> SubRecord r1 -> Boolean
unsafeHas = runFn2 unsafeHasFn