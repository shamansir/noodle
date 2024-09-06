module Noodle.Patch where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Tuple.Nested ((/\), type (/\))

import Prim.Row as R
import Prim.RowList as RL
-- import Type.RowList as RL
-- import Type.Row as R

import Noodle.Id (PatchR, FamilyR, NodeR) as Id
import Noodle.Node (Node, RawNode)
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families)
import Noodle.Node.HoldsNode (HoldsNode, HoldsRawNode)
import Noodle.Link (RawLink)
import Noodle.Link (Id, FromId, ToId) as Link


data Patch state (families :: Families) repr m =
  Patch
    String
    Id.PatchR
    state
    (Map Id.FamilyR (Array (HoldsNode repr m)))
    (Map Id.FamilyR (Array (HoldsRawNode repr m)))
    Links


type Links =
  { lastId :: Link.Id
  , from :: Map Link.FromId RawLink
  , to :: Map Link.ToId RawLink
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Link.Id RawLink
  }
