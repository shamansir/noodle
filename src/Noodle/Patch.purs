module Noodle.Patch where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Tuple.Nested ((/\), type (/\))

import Prim.Row as R
import Prim.RowList as RL
-- import Type.RowList as RL
-- import Type.Row as R

import Noodle.Id (PatchR, FamilyR, NodeR, Link) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families)
import Noodle.Node.HoldsNode (HoldsNode)
import Noodle.Link (FromId, ToId) as Link
import Noodle.Raw.Link (Link) as Raw


data Patch state (families :: Families) repr m =
  Patch
    String
    Id.PatchR
    state
    (Map Id.FamilyR (Array (HoldsNode repr m)))
    (Map Id.FamilyR (Array (Raw.Node repr m)))
    Links


type Links =
  { lastId :: Id.Link
  , from :: Map Link.FromId Raw.Link
  , to :: Map Link.ToId Raw.Link
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Id.Link Raw.Link
  }
