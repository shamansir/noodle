module Noodle.Toolkit.HoldsFamily where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Noodle.Toolkit.Families (Family, RawFamily)


newtype HoldsFamily repr m = HoldsFamily (forall r. (forall f state is os. IsSymbol f => Family f state is os repr m -> r) -> r)


holdFamily :: forall f state is os repr m. IsSymbol f => Family f state is os repr m -> HoldsFamily repr m
holdFamily family = HoldsFamily (_ $ family)


withFamily :: forall r repr m. HoldsFamily repr m -> (forall f state is os. IsSymbol f => Family f state is os repr m -> r) -> r
withFamily (HoldsFamily f) = f


newtype HoldsRawFamily repr m = HoldsRawFamily (forall r. (forall state. RawFamily state repr m -> r) -> r)


holdRawFamily :: forall state repr m. RawFamily state repr m -> HoldsRawFamily repr m
holdRawFamily family = HoldsRawFamily (_ $ family)


withRawFamily :: forall r repr m. HoldsRawFamily repr m -> (forall state. RawFamily state repr m -> r) -> r
withRawFamily (HoldsRawFamily f) = f
