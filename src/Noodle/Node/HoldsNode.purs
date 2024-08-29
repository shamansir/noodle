module Noodle.Node.HoldsNode where

import Prelude

import Data.Symbol (class IsSymbol)

import Noodle.Node (Node, RawNode)


newtype HoldsNode repr m = HoldsNode (forall r. (forall f state is os. IsSymbol f => Node f state is os repr m -> r) -> r)


holdNode :: forall f state is os repr m. IsSymbol f => Node f state is os repr m -> HoldsNode repr m
holdNode node = HoldsNode (_ $ node)


withNode :: forall r repr m. HoldsNode repr m -> (forall f state is os. IsSymbol f => Node f state is os repr m -> r) -> r
withNode (HoldsNode f) = f


newtype HoldsRawNode repr m = HoldsRawNode (forall r. (forall state. RawNode state repr m -> r) -> r)


holdRawNode :: forall state repr m. RawNode state repr m -> HoldsRawNode repr m
holdRawNode node = HoldsRawNode (_ $ node)


withRawNode :: forall r repr m. HoldsRawNode repr m -> (forall state. RawNode state repr m -> r) -> r
withRawNode (HoldsRawNode f) = f
