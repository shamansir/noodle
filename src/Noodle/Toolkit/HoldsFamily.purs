module Noodle.Toolkit.HoldsFamily where

import Prelude

import Data.Symbol (class IsSymbol)

import Type.Proxy (Proxy)

import Noodle.Toolkit.Families (F, Families, class RegisteredFamily)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Toolkit.Family (Family)


newtype HoldsFamily strepr chrepr m = HoldsFamily (forall r. (forall f fstate is os. IsSymbol f => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r)


holdFamily :: forall f fstate strepr is os chrepr m. IsSymbol f => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> HoldsFamily strepr chrepr m
holdFamily family = HoldsFamily (_ $ family)


withFamily :: forall r strepr chrepr m. HoldsFamily strepr chrepr m -> (forall f fstate is os. IsSymbol f => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r
withFamily (HoldsFamily f) = f


newtype HoldsFamilyFS strepr chrepr m (fs :: Families) = HoldsFamilyFS (forall r. (forall f fstate is os. IsSymbol f => RegisteredFamily (F f fstate is os chrepr m) fs => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r)


holdFamilyFS :: forall f fs fstate strepr is os chrepr m. IsSymbol f => HasFallback fstate => RegisteredFamily (F f fstate is os chrepr m) fs => StRepr fstate strepr => Proxy fs -> Family f fstate is os chrepr m -> HoldsFamilyFS strepr chrepr m fs
holdFamilyFS _ family = HoldsFamilyFS (_ $ family)


withFamilyFS :: forall r fs strepr chrepr m. HoldsFamilyFS strepr chrepr m fs -> (forall f fstate is os. IsSymbol f => RegisteredFamily (F f fstate is os chrepr m) fs => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r
withFamilyFS (HoldsFamilyFS f) = f