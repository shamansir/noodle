module Noodle.Patch where

import Prelude

import Prim.Boolean (True, False)
import Prim.Row as R
import Prim.RowList as RL

import Type.Data.List (class IsMember)
import Type.Data.List.Extra (class LMap, class MapDown, mapDown)
import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (empty, alter, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (generate) as UH
import Data.Array (singleton, cons, concat, catMaybes) as Array

import Unsafe.Coerce (unsafeCoerce)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Signal.Channel (Channel, channel)

import Noodle.Id (PatchR, Family, FamilyR, NodeR, Link, PatchName, PatchR, patchR, familyR, Inlet, Outlet) as Id
import Noodle.Link (FromId, ToId, setId, cancel) as Link
import Noodle.Link (Link)
import Noodle.Node (Node)
import Noodle.Node (family, toRaw, connect) as Node
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Node.HoldsNode (HoldsNode, holdNode)
import Noodle.Node.HoldsNode (withNode) as HN
import Noodle.Patch.Links (Links)
import Noodle.Patch.Links (init, track, nextId, forget, forgetRaw, findRaw) as Links
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (cancel) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (family, toReprableState) as RawNode
import Noodle.Repr (class ToRepr, class FromRepr, class FromToRepr)
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Wiring (class Wiring)



data Patch state (families :: Families) repr m =
  Patch
    Id.PatchName
    Id.PatchR
    -- Toolkit families repr m
    (Channel state)
    (Map Id.FamilyR (Array (HoldsNode repr m))) -- FIXME: consider storing all the nodes in Raw format since, all the type data is in `families :: Families` and can be extracted
    (Map Id.FamilyR (Array (Raw.Node repr repr m))) -- use `Channel` as well?
    Links -- use `Channel` as well?


make :: forall m state families repr mp. MonadEffect m => Id.PatchName -> state -> m (Patch state families repr mp)
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
      Links.init


fromToolkit :: forall m tk state families repr mp. MonadEffect m => Toolkit tk families repr mp -> Id.PatchName -> state -> m (Patch state families repr mp)
fromToolkit _ = make


name :: forall state families repr m. Patch state families repr m -> Id.PatchName
name (Patch n _ _ _ _ _) = n


id :: forall state families repr m. Patch state families repr m -> Id.PatchR
id (Patch _ i _ _ _ _) = i


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
registerNodeNotFromToolkit node (Patch name id chState nodes rawNodes links) =
    Patch name id chState (Map.alter (insertOrInit $ holdNode node) (Id.familyR $ Node.family node) nodes) rawNodes links
    where
      insertOrInit :: HoldsNode repr m -> Maybe (Array (HoldsNode repr m)) -> Maybe (Array (HoldsNode repr m))
      insertOrInit holdsNode Nothing      = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


registerRawNode
    :: forall pstate nstate repr m families
     . FromToRepr nstate repr
    => Raw.Node nstate repr m
    -> Patch pstate families repr m
    -> Patch pstate families repr m
registerRawNode rawNode (Patch name id chState nodes rawNodes links) =
    Patch name id chState nodes (Map.alter (insertOrInit $ RawNode.toReprableState rawNode) (RawNode.family rawNode) rawNodes) links
    where
      insertOrInit :: Raw.Node repr repr m -> Maybe (Array (Raw.Node repr repr m)) -> Maybe (Array (Raw.Node repr repr m))
      insertOrInit holdsNode Nothing        = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


connect
    :: forall m state repr mp families fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA'
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => FromRepr repr doutA
    => ToRepr dinB repr
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => RegisteredFamily (F fA stateA isA osA repr mp) families
    => RegisteredFamily (F fB stateB isB osB repr mp) families
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA stateA isA osA repr mp
    -> Node fB stateB isB osB repr mp
    -> Patch state families repr mp
    -> m (Patch state families repr mp /\ Link fA fB oA iB)
connect outletA inletB nodeA nodeB (Patch name id chState nodes rawNodes links) = do
    link <- Node.connect outletA inletB nodeA nodeB
    let
      linkWithId = link # Link.setId (Links.nextId links)
      nextLinks = links # Links.track linkWithId
      nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ linkWithId)


disconnect
    :: forall m state repr mp families fA fB oA iB
     . Wiring m
    => Link fA fB oA iB
    -> Patch state families repr mp
    -> m (Patch state families repr mp /\ Boolean)
disconnect link (Patch name id chState nodes rawNodes links) = do
    -- FIXME: ensure link is registered in the patch. Return false if not
    _ <- liftEffect $ Link.cancel link
    let
        nextLinks = links # Links.forget link
        nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ true)
    {-
    link <- Node.connect outletA inletB nodeA nodeB
    let
      linkWithId = link # Link.setId (Links.nextId links)
      nextLinks = links # Links.track linkWithId
      nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ linkWithId) -}


disconnectRaw
    :: forall m state repr mp families
     . Wiring m
    => Raw.Link
    -> Patch state families repr mp
    -> m (Patch state families repr mp /\ Boolean)
disconnectRaw rawLink (Patch name id chState nodes rawNodes links) = do
    -- FIXME: ensure link is registered in the patch. Return false if not
    _ <- liftEffect $ RawLink.cancel rawLink
    let
        nextLinks = links # Links.forgetRaw rawLink
        nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ true)


findRawLink
    :: forall state repr mp families
     . Id.Link
    -> Patch state families repr mp
    -> Maybe Raw.Link
findRawLink linkId (Patch _ _ _ _ _ links) =
    links # Links.findRaw linkId


data MapNodes repr m = MapNodes (Map Id.FamilyR (Array (HoldsNode repr m)))


instance IsSymbol f => LMap (MapNodes repr m) (F f state is os repr m) (Maybe (Array (HoldsNode repr m))) where
    lmap :: MapNodes repr m -> Proxy (F f state is os repr m) -> Maybe (Array (HoldsNode repr m))
    lmap (MapNodes families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


class MapNodesImpl :: Type -> (Type -> Type) -> Families -> Constraint
class    (MapDown (MapNodes repr m) families Array (Maybe (Array (HoldsNode repr m)))) <= MapNodesImpl repr m families
instance (MapDown (MapNodes repr m) families Array (Maybe (Array (HoldsNode repr m)))) => MapNodesImpl repr m families


mapNodes
    :: forall x pstate families repr m
    .  MapNodesImpl repr m families
    => (forall f state is os. IsSymbol f => FromToRepr state repr => Node f state is os repr m -> x)
    -> Patch pstate families repr m
    -> Array x
mapNodes f (Patch _ _ _ nodes _ _) =
    Array.concat $
      (map nodeToX)
        <$> Array.catMaybes
              (mapDown (MapNodes nodes) (Proxy :: _ families) :: Array (Maybe (Array (HoldsNode repr m))))
    where nodeToX hn = HN.withNode hn f


mapRawNodes
    :: forall x pstate families repr m
    .  (Raw.Node repr repr m -> x)
    -> Patch pstate families repr m
    -> Array x
mapRawNodes f (Patch _ _ _ _ rawNodes _) =
    Array.concat $ Map.toUnfoldable rawNodes <#> Tuple.snd <#> map f


mapAllNodes
    :: forall x pstate families repr m
    .  (Raw.Node repr repr m -> x)
    -> Patch pstate families repr m
    -> Array x
mapAllNodes f patch@(Patch _ _ _ nodes _ _) =
    Array.concat (map (toRawCnv >>> f) <$> Tuple.snd <$> Map.toUnfoldable nodes) <> mapRawNodes f patch
    where
      toRawCnv hn = HN.withNode hn (Node.toRaw >>> RawNode.toReprableState)


withNodes
    :: forall f state is os x pstate families repr m
    .  RegisteredFamily (F f state is os repr m) families
    => IsSymbol f
    => (Node f state is os repr m -> x)
    -> Id.Family f
    -> Patch pstate families repr m
    -> Array x
withNodes f familyId (Patch _ _ _ nodes _ _) =
    Map.lookup (Id.familyR familyId) nodes <#> map nodeToX # fromMaybe []
    where nodeToX hn = HN.withNode hn (unsafeCoerce >>> f)


withRawNodes
    :: forall x pstate families repr m
    .  (Raw.Node repr repr m -> x)
    -> Id.FamilyR
    -> Patch pstate families repr m
    -> Array x
withRawNodes f familyR (Patch _ _ _ _ rawNodes _) =
    (Map.lookup familyR rawNodes <#> map f) # fromMaybe []


withAnyNodes
    :: forall x pstate families repr m
    .  (Raw.Node repr repr m -> x)
    -> Id.FamilyR
    -> Patch pstate families repr m
    -> Array x
withAnyNodes f familyR (Patch _ _ _ nodes rawNodes _) =
    case Map.lookup familyR rawNodes of
        Just rawNodesOfF -> f <$> rawNodesOfF
        Nothing ->
            case Map.lookup familyR nodes of
                Just holdsNodeArr -> map (toRawCnv >>> f) holdsNodeArr
                Nothing -> []
    where
        toRawCnv hn = HN.withNode hn (Node.toRaw >>> RawNode.toReprableState)