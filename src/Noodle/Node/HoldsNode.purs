module Noodle.Node.HoldsNode where

import Prelude

import Data.Symbol (class IsSymbol)

import Noodle.Node (Node)
import Noodle.Repr.ChRepr (class ToRepr, class FromRepr)


newtype HoldsNode strepr chrepr m = HoldsNode (forall r. (forall f state is os. IsSymbol f => StRepr state strepr => Node f state is os chrepr m -> r) -> r)


holdNode :: forall f state is os chrepr m. IsSymbol f => StRepr state strepr => Node f state is os chrepr m -> HoldsNode strepr chrepr m
holdNode node = HoldsNode (_ $ node)


withNode :: forall r strepr chrepr m. HoldsNode strepr chrepr m -> (forall f state is os. IsSymbol f => StRepr state strepr => Node f state is os chrepr m -> r) -> r
withNode (HoldsNode f) = f