module Noodle.Patch where

import Prelude

import Data.Symbol (class IsSymbol)

import Prim.Row as R
import Prim.RowList as RL
-- import Type.RowList as RL
-- import Type.Row as R

import Noodle.Id (PatchR) as Id
import Noodle.Node (Node, RawNode)
import Noodle.Toolkit (Toolkit)


newtype HoldsNode repr m = HoldsNode (forall r. (forall f state is os. IsSymbol f => Node f state is os repr m -> r) -> r)


holdNode :: forall f state is os repr m. IsSymbol f => Node f state is os repr m -> HoldsNode repr m
holdNode node = HoldsNode (_ $ node)


withNode :: forall r repr m. HoldsNode repr m -> (forall f state is os. IsSymbol f => Node f state is os repr m -> r) -> r
withNode (HoldsNode f) = f


newtype HoldsRawNode repr m = HoldsRawNode (forall r. (forall state. RawNode state repr m -> r) -> r)


holdRawNode :: forall state repr m. RawNode state repr m -> HoldsRawNode repr m
holdRawNode node = HoldsRawNode (_ $ node)


withRawNode :: forall r repr m. HoldsNode repr m -> (forall f state is os. IsSymbol f => Node f state is os repr m -> r) -> r
withRawNode (HoldsNode f) = f


{- type Links =
  { lastId :: LinkId
  , from :: Map Node.FromId HoldsLink
  , to :: Map Node.ToId HoldsLink
  , byNode :: Map Id.NodeIdR (Array (Node.FromId /\ Node.ToId))
  , byId :: Map LinkId HoldsLink
  } -}


data Patch state (tookit :: Toolkit) repr m = Patch String Id.PatchR state (Array (HoldsRawNode repr m)) {- Links -}