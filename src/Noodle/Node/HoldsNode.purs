module Noodle.Node.HoldsNode where

import Prelude

import Data.Symbol (class IsSymbol)

import Noodle.Node (Node)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)


newtype HoldsNode strepr chrepr m = HoldsNode (forall r. (forall f state is os. IsSymbol f => HasFallback state => StRepr state strepr => Node f state is os chrepr m -> r) -> r)


holdNode :: forall f state strepr is os chrepr m. IsSymbol f => HasFallback state => StRepr state strepr => Node f state is os chrepr m -> HoldsNode strepr chrepr m
holdNode node = HoldsNode (_ $ node)


withNode :: forall r strepr chrepr m. HoldsNode strepr chrepr m -> (forall f state is os. IsSymbol f => HasFallback state => StRepr state strepr => Node f state is os chrepr m -> r) -> r
withNode (HoldsNode f) = f