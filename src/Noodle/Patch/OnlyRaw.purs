module Noodle.OnlyRawPatch where

import Prelude

import Signal.Channel (Channel, channel)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Type.Data.List (class IsMember)
import Type.Data.List.Extra (class LMap, class MapDown, mapDown)
import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (empty, alter, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (generate) as UH
import Data.Array (singleton, cons, concat, catMaybes) as Array
import Data.Repr (class ToRepr, class FromRepr)

import Prim.Boolean (True, False)
import Prim.Row as R
import Prim.RowList as RL
-- import Type.RowList as RL
-- import Type.Row as R

import Noodle.Id (PatchR, FamilyR, NodeR, Link, PatchName, PatchR, patchR, familyR, Inlet, Outlet) as Id
import Noodle.Node (Node)
import Noodle.Node (family, toRaw, connect) as Node
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (family) as RawNode
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Node.HoldsNode (HoldsNode, holdNode, withNode)
import Noodle.Link (Link)
import Noodle.Link (FromId, ToId, setId, cancel) as Link
import Noodle.Raw.Link (Link) as Raw
import Noodle.Patch.Links (Links)
import Noodle.Patch.Links (init, track, nextId, forget) as Links
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Wiring (class Wiring)


data Patch state (families :: Families) repr m =
  Patch
    Id.PatchName
    Id.PatchR
    -- Toolkit families repr m
    (Channel state)
    (Map Id.FamilyR (Array (Raw.Node repr m))) -- use `Channel` as well?
    Links -- use `Channel` as well?


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
      Links.init


fromToolkit :: forall state families repr m. MonadEffect m => Toolkit families repr m -> Id.PatchName -> state -> m (Patch state families repr m)
fromToolkit _ = make


registerNode
    :: forall f state repr is os m families
     . IsSymbol f
    => MonadEffect m
    => FromRepr repr state => ToRepr state repr
    => RegisteredFamily (F f state is os repr m) families
    => Node f state is os repr m
    -> Patch state families repr m
    -> Patch state families repr m
registerNode =
    registerNodeNotFromToolkit -- it just has no `IsMember` constraint


registerNodeNotFromToolkit
    :: forall f state repr is os m families
     . IsSymbol f
    => MonadEffect m
    => FromRepr repr state => ToRepr state repr
    => Node f state is os repr m
    -> Patch state families repr m
    -> Patch state families repr m
registerNodeNotFromToolkit node =
    registerRawNode (Node.toRaw node)


registerRawNode
    :: forall state repr m families
     . Raw.Node repr m
    -> Patch state families repr m
    -> Patch state families repr m
registerRawNode rawNode (Patch name id chState rawNodes links) =
    Patch name id chState (Map.alter (insertOrInit rawNode) (RawNode.family rawNode) rawNodes) links
    where
      insertOrInit :: Raw.Node repr m -> Maybe (Array (Raw.Node repr m)) -> Maybe (Array (Raw.Node repr m))
      insertOrInit holdsNode Nothing        = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


connect
    :: forall state repr m families fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA'
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromRepr repr doutA
    => ToRepr dinB repr
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => RegisteredFamily (F fA stateA isA osA repr m) families
    => RegisteredFamily (F fB stateB isB osB repr m) families
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA repr m
    -> Node fB stateB isB osB repr m
    -> Patch state families repr m
    -> m (Patch state families repr m /\ Link fA fB oA iB)
connect outletA inletB nodeA nodeB (Patch name id chState rawNodes links) = do
    link <- Node.connect outletA inletB nodeA nodeB
    let
      linkWithId = link # Link.setId (Links.nextId links)
      nextLinks = links # Links.track linkWithId
      nextPatch = Patch name id chState rawNodes nextLinks
    pure (nextPatch /\ linkWithId)


disconnect
    :: forall state repr m families fA fB oA iB
     . Wiring m
    => Link fA fB oA iB
    -> Patch state families repr m
    -> m (Patch state families repr m /\ Boolean)
disconnect link (Patch name id chState rawNodes links) = do
    _ <- liftEffect $ Link.cancel link
    let
        nextLinks = links # Links.forget link
        nextPatch = Patch name id chState rawNodes nextLinks
    pure (nextPatch /\ true)
    {-
    link <- Node.connect outletA inletB nodeA nodeB
    let
      linkWithId = link # Link.setId (Links.nextId links)
      nextLinks = links # Links.track linkWithId
      nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ linkWithId) -}


data MapNodes repr m = MapNodes (Map Id.FamilyR (Array (HoldsNode repr m)))


instance IsSymbol f => LMap (MapNodes repr m) (F f state is os repr m) (Maybe (Array (HoldsNode repr m))) where
    lmap :: MapNodes repr m -> Proxy (F f state is os repr m) -> Maybe (Array (HoldsNode repr m))
    lmap (MapNodes families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


mapNodes
    :: forall x pstate families repr m
    .  (Raw.Node repr m -> x)
    -> Patch pstate families repr m
    -> Array x
mapNodes f (Patch _ _ _ rawNodes _) =
    Array.concat $ Map.toUnfoldable rawNodes <#> Tuple.snd <#> map f