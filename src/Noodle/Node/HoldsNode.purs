module Noodle.Node.HoldsNode where

import Prelude

import Data.Symbol (class IsSymbol)
import Noodle.Repr (class ToRepr, class FromRepr)

import Noodle.Node (Node)


newtype HoldsNode repr m = HoldsNode (forall r. (forall f state is os. IsSymbol f => FromRepr repr state => ToRepr state repr => Node f state is os repr m -> r) -> r)


holdNode :: forall f state is os repr m. IsSymbol f => FromRepr repr state => ToRepr state repr => Node f state is os repr m -> HoldsNode repr m
holdNode node = HoldsNode (_ $ node)


withNode :: forall r repr m. HoldsNode repr m -> (forall f state is os. IsSymbol f => FromRepr repr state => ToRepr state repr => Node f state is os repr m -> r) -> r
withNode (HoldsNode f) = f