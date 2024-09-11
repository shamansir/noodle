module Noodle.Patch where

import Prelude

import Signal.Channel (Channel, channel)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Type.Data.List (class IsMember)

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (empty, alter) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (generate) as UH
import Data.Array (singleton, cons) as Array

import Prim.Boolean (True, False)
import Prim.Row as R
import Prim.RowList as RL
-- import Type.RowList as RL
-- import Type.Row as R

import Noodle.Id (PatchR, FamilyR, NodeR, Link, PatchName, PatchR, patchR, familyR) as Id
import Noodle.Node (Node)
import Noodle.Node (family) as Node
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (family) as RawNode
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families, F)
import Noodle.Node.HoldsNode (HoldsNode, holdNode)
import Noodle.Link (FromId, ToId) as Link
import Noodle.Raw.Link (Link) as Raw


data Patch state (families :: Families) repr m =
  Patch
    Id.PatchName
    Id.PatchR
    -- Toolkit families repr m
    (Channel state)
    (Map Id.FamilyR (Array (HoldsNode repr m)))
    (Map Id.FamilyR (Array (Raw.Node repr m)))
    Links


type Links = -- TODO: separate module
  { lastId :: Maybe Id.Link
  , from :: Map Link.FromId Raw.Link
  , to :: Map Link.ToId Raw.Link
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Id.Link Raw.Link
  }


make :: forall state families repr m. MonadEffect m => Id.PatchName -> state -> m (Patch state families repr m)
make patchName state = liftEffect $ do
  uniqueHash <- UH.generate
  let patchId = Id.patchR uniqueHash
  stateCh <- channel state
  pure
    $ Patch
      patchName
      patchId
      stateCh
      Map.empty
      Map.empty
      initLinks


registerNode
    :: forall f state repr is os m families
     . IsSymbol f
    => MonadEffect m
    => IsMember (F f state is os repr m) families True
    => Node f state is os repr m
    -> Patch state families repr m
    -> Patch state families repr m
registerNode node (Patch name id chState nodes rawNodes links) =
    Patch name id chState (Map.alter (insertOrInit $ holdNode node) (Id.familyR $ Node.family node) nodes) rawNodes links
    where
      insertOrInit :: HoldsNode repr m -> Maybe (Array (HoldsNode repr m)) -> Maybe (Array (HoldsNode repr m))
      insertOrInit holdsNode Nothing      = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


registerRawNode
    :: forall state repr m families
     . Raw.Node repr m
    -> Patch state families repr m
    -> Patch state families repr m
registerRawNode rawNode (Patch name id chState nodes rawNodes links) =
    Patch name id chState nodes (Map.alter (insertOrInit rawNode) (RawNode.family rawNode) rawNodes) links
    where
      insertOrInit :: Raw.Node repr m -> Maybe (Array (Raw.Node repr m)) -> Maybe (Array (Raw.Node repr m))
      insertOrInit holdsNode Nothing      = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


initLinks :: Links
initLinks =
  { lastId : Nothing
  , from : Map.empty
  , to : Map.empty
  , byNode : Map.empty
  , byId : Map.empty
  }