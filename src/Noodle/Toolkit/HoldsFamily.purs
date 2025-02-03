module Noodle.Toolkit.HoldsFamily where

import Prelude

import Data.Symbol (class IsSymbol)

import Type.Proxy (Proxy)

import Noodle.Toolkit.Families (F, Families, class RegisteredFamily)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Toolkit.Family (Family)


newtype HoldsFamily strepr chrepr m (fs :: Families) = HoldsFamily (forall r. (forall f fstate is os. IsSymbol f => RegisteredFamily (F f fstate is os chrepr m) fs => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r)


holdFamily :: forall f fs fstate strepr is os chrepr m. IsSymbol f => HasFallback fstate => RegisteredFamily (F f fstate is os chrepr m) fs => StRepr fstate strepr => Proxy fs -> Family f fstate is os chrepr m -> HoldsFamily strepr chrepr m fs
holdFamily _ family = HoldsFamily (_ $ family)


withFamily :: forall r fs strepr chrepr m. HoldsFamily strepr chrepr m fs -> (forall f fstate is os. IsSymbol f => RegisteredFamily (F f fstate is os chrepr m) fs => HasFallback fstate => StRepr fstate strepr => Family f fstate is os chrepr m -> r) -> r
withFamily (HoldsFamily f) = f