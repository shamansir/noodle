module Noodle.Toolkit.HoldsFamily where

import Prelude

import Data.Symbol (class IsSymbol)

import Noodle.Repr.StRepr (class StRepr)
import Noodle.Toolkit.Family (Family)


newtype HoldsFamily strepr chrepr m = HoldsFamily (forall r. (forall f state is os. IsSymbol f => StRepr strepr state => Family f state is os chrepr m -> r) -> r)


holdFamily :: forall f state strepr is os chrepr m. IsSymbol f => StRepr strepr state => Family f state is os chrepr m -> HoldsFamily strepr chrepr m
holdFamily family = HoldsFamily (_ $ family)


withFamily :: forall r strepr chrepr m. HoldsFamily strepr chrepr m -> (forall f state is os. IsSymbol f => StRepr strepr state => Family f state is os chrepr m -> r) -> r
withFamily (HoldsFamily f) = f