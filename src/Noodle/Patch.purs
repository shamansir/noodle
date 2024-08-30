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
import Noodle.Toolkit.Families (Families)
import Noodle.Node.HoldsNode (HoldsRawNode)


{- type Links =
  { lastId :: LinkId
  , from :: Map Node.FromId HoldsLink
  , to :: Map Node.ToId HoldsLink
  , byNode :: Map Id.NodeIdR (Array (Node.FromId /\ Node.ToId))
  , byId :: Map LinkId HoldsLink
  } -}


data Patch state (families :: Families) repr m = Patch String Id.PatchR state (Array (HoldsRawNode repr m)) {- Links -}