module Noodle.Toolkit.HoldsFamily where

import Prelude

import Data.Symbol (class IsSymbol)

import Noodle.Repr (class FromToRepr)
import Noodle.Toolkit.Family (Family)


newtype HoldsFamily repr m = HoldsFamily (forall r. (forall f state is os. IsSymbol f => FromToRepr state repr => Family f state is os repr m -> r) -> r)


holdFamily :: forall f state is os repr m. IsSymbol f => FromToRepr state repr => Family f state is os repr m -> HoldsFamily repr m
holdFamily family = HoldsFamily (_ $ family)


withFamily :: forall r repr m. HoldsFamily repr m -> (forall f state is os. IsSymbol f => FromToRepr state repr => Family f state is os repr m -> r) -> r
withFamily (HoldsFamily f) = f