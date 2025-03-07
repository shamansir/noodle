module Noodle.Patch where

import Prelude

import Prim.Boolean (True, False)
import Prim.Row as R
import Prim.RowList as RL

import Debug as Debug

import Type.Data.List (class IsMember)
import Type.Data.List.Extra (class LMap, class MapDown, mapDown)
import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (empty, alter, lookup, toUnfoldable, update) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (generate) as UH
import Data.Array (singleton, cons, concat, catMaybes, find, filter, length) as Array
import Data.Traversable (traverse_)

import Unsafe.Coerce (unsafeCoerce)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Signal (Signal)
import Signal (get) as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel (subscribe) as Channel

import Noodle.Id (PatchR, Family, FamilyR, NodeR, LinkR, PatchName, PatchR, patchR, familyR, familyOf, Inlet, Outlet, InletR, OutletR) as Id
import Noodle.Link (FromId, ToId, cancel) as Link
import Noodle.Link (Link)
import Noodle.Node (Node)
import Noodle.Node (id, family, toRaw, connect) as Node
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Node.HoldsNode (HoldsNode, holdNode)
import Noodle.Node.HoldsNode (withNode) as HN
import Noodle.Raw.Node (connect) as RawNode
import Noodle.Patch.Links (Links)
import Noodle.Patch.Links (init, track, forget, forgetRaw, findRaw, trackRaw, findAllFrom, findAllTo, forgetAllFrom, forgetAllTo, all) as Links
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (cancel, id) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, family, toReprableState) as RawNode
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (class FromValueInChannel, class ToValueInChannel, class FromToValueInChannel)
import Noodle.Repr.ValueInChannel (accept) as ViC
import Noodle.Repr.Tagged (class Tagged) as CT
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Wiring (class Wiring)


data Patch state (families :: Families) strepr chrepr m =
  Patch
    Id.PatchName
    Id.PatchR
    -- Toolkit families repr m
    (Channel state)
    (Map Id.FamilyR (Array (HoldsNode strepr chrepr m))) -- FIXME: consider storing all the nodes in Raw format since, all the type data is in `families :: Families` and can be extracted. The only problem is may be spawning new nodes?
    (Map Id.FamilyR (Array (Raw.Node strepr chrepr m))) -- use `Channel` as well?
    Links -- use `Channel` as well?


make :: forall m state families strepr chrepr mp. MonadEffect m => Id.PatchName -> state -> m (Patch state families strepr chrepr mp)
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


fromToolkit :: forall m tk state families strepr chrepr mp. MonadEffect m => Toolkit tk families strepr chrepr mp -> Id.PatchName -> state -> m (Patch state families strepr chrepr mp)
fromToolkit _ = make


name :: forall state families strepr chrepr m. Patch state families strepr chrepr m -> Id.PatchName
name (Patch n _ _ _ _ _) = n


id :: forall state families strepr chrepr m. Patch state families strepr chrepr m -> Id.PatchR
id (Patch _ i _ _ _ _) = i


registerNode
    :: forall f pstate fstate strepr chrepr is os m families
     . IsSymbol f
    => MonadEffect m
    => HasFallback fstate
    => StRepr fstate strepr
    => RegisteredFamily (F f fstate is os chrepr m) families
    => Node f fstate is os chrepr m
    -> Patch pstate families strepr chrepr m
    -> Patch pstate families strepr chrepr m
registerNode =
    registerGivenNode -- it just has no `IsMember` constraint


registerGivenNode
    :: forall pstate f fstate strepr chrepr is os m families
     . IsSymbol f
    => MonadEffect m
    => HasFallback fstate
    => StRepr fstate strepr
    => Node f fstate is os chrepr m
    -> Patch pstate families strepr chrepr m
    -> Patch pstate families strepr chrepr m
registerGivenNode node (Patch name id chState nodes rawNodes links) =
    Patch name id chState (Map.alter (insertOrInit $ holdNode node) (Id.familyR $ Node.family node) nodes) rawNodes links
    where
      insertOrInit :: HoldsNode strepr chrepr m -> Maybe (Array (HoldsNode strepr chrepr m)) -> Maybe (Array (HoldsNode strepr chrepr m))
      insertOrInit holdsNode Nothing      = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


registerRawNode
    :: forall pstate strepr chrepr m families
     . Raw.Node strepr chrepr m
    -> Patch pstate families strepr chrepr m
    -> Patch pstate families strepr chrepr m
registerRawNode rawNode (Patch name id chState nodes rawNodes links) =
    Patch name id chState nodes (Map.alter (insertOrInit rawNode) (RawNode.family rawNode) rawNodes) links
    where
      insertOrInit :: Raw.Node strepr chrepr m -> Maybe (Array (Raw.Node strepr chrepr m)) -> Maybe (Array (Raw.Node strepr chrepr m))
      insertOrInit holdsNode Nothing        = Just $ Array.singleton holdsNode
      insertOrInit holdsNode (Just prev_vs) = Just $ Array.cons holdsNode prev_vs


registerRawNode'
    :: forall pstate fstate strepr chrepr m families
     . HasFallback fstate
    => StRepr fstate strepr
    => Raw.Node fstate chrepr m
    -> Patch pstate families strepr chrepr m
    -> Patch pstate families strepr chrepr m
registerRawNode' =
    registerRawNode <<< RawNode.toReprableState


-- FIXME: not the fastest way, use `Map.lookup`, but anyway we need to unfold it since it is grouped by family
findNode :: forall pstate strepr chrepr m families. StoresNodesAt strepr chrepr m families => Id.NodeR -> Patch pstate families strepr chrepr m -> Maybe (HoldsNode strepr chrepr m)
findNode nodeR = nonRawNodes >>> Array.find (\hn -> HN.withNode hn (Node.id >>> (_ == nodeR)))


 -- FIXME: not the fastest way, use `Map.lookup`, but anyway we need to unfold it since it is grouped by family
findRawNode :: forall pstate strepr chrepr m families. Id.NodeR -> Patch pstate families strepr chrepr m -> Maybe (Raw.Node strepr chrepr m)
findRawNode nodeR = mapAllNodes identity >>> Array.find (RawNode.id >>> (_ == nodeR))


connect
    :: forall m pstate strepr chrepr mp families fA fB oA iB doutA dinB fstateA fstateB isA isB isB' osA osB osA'
     . Wiring m
    => IsSymbol fA
    => IsSymbol fB
    => HasFallback chrepr
    => FromValueInChannel dinB chrepr
    => ToValueInChannel chrepr dinB
    => HasOutlet osA osA' oA doutA
    => HasInlet isB isB' iB dinB
    => RegisteredFamily (F fA fstateA isA osA chrepr mp) families
    => RegisteredFamily (F fB fstateB isB osB chrepr mp) families
    => Id.Outlet oA
    -> Id.Inlet iB
    -> Node fA fstateA isA osA chrepr mp
    -> Node fB fstateB isB osB chrepr mp
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp /\ Link fA fB oA iB)
connect outletA inletB nodeA nodeB (Patch name id chState nodes rawNodes links) = do
    link <- Node.connect outletA inletB nodeA nodeB
    let
      nextLinks = links # Links.track link
      nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ link)


connectRaw
    :: forall m pstate strepr chrepr mp families fstateA fstateB
     . Wiring m
    => CT.Tagged chrepr
    => Id.OutletR
    -> Id.InletR
    -> Raw.Node fstateA chrepr mp
    -> Raw.Node fstateB chrepr mp
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp /\ Raw.Link)
connectRaw outletRA inletRB nodeRA nodeRB (Patch name id chState nodes rawNodes links) = do
    rawLink <- RawNode.connect outletRA inletRB identity nodeRA nodeRB
    -- let _ = Debug.spy "connect raw" $ RawLink.id rawLink
    let
      nextLinks = links # Links.trackRaw rawLink
      nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ rawLink)


disconnect
    :: forall m pstate strepr chrepr mp families fA fB oA iB
     . Wiring m
    => Link fA fB oA iB
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp /\ Boolean)
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
    :: forall m pstate strepr chrepr mp families
     . Wiring m
    => Raw.Link
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp /\ Boolean)
disconnectRaw rawLink (Patch name id chState nodes rawNodes links) = do
    -- FIXME: ensure link is registered in the patch. Return false if not
    -- let _ = Debug.spy "disconnect raw" $ RawLink.id rawLink
    _ <- liftEffect $ RawLink.cancel rawLink
    let
        nextLinks = links # Links.forgetRaw rawLink
        nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure (nextPatch /\ true)


disconnectAllFromTo
    :: forall m pstate strepr chrepr mp families
     . Wiring m
    => Id.NodeR
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp)
disconnectAllFromTo nodeR patch =
    disconnectAllFrom nodeR patch >>= disconnectAllTo nodeR


disconnectAllFrom
    :: forall m pstate strepr chrepr mp families
     . Wiring m
    => Id.NodeR
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp)
disconnectAllFrom nodeR (Patch name id chState nodes rawNodes links) = do
    -- let _ = Debug.spy "disconnect all from" $ show nodeR
    let (nextLinks /\ allLinksFrom) = Links.forgetAllFrom nodeR links
    -- let _ = Debug.spy "links count from" $ show $ Array.length allLinksFrom
    liftEffect $ traverse_ RawLink.cancel allLinksFrom
    let nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure nextPatch


disconnectAllTo
    :: forall m pstate strepr chrepr mp families
     . Wiring m
    => Id.NodeR
    -> Patch pstate families strepr chrepr mp
    -> m (Patch pstate families strepr chrepr mp)
disconnectAllTo nodeR (Patch name id chState nodes rawNodes links) = do
    -- let _ = Debug.spy "disconnect all to" $ show nodeR
    let (nextLinks /\ allLinksTo)  = Links.forgetAllTo nodeR links
    -- let _ = Debug.spy "links count to" $ show $ Array.length allLinksTo
    liftEffect $ traverse_ RawLink.cancel allLinksTo
    let nextPatch = Patch name id chState nodes rawNodes nextLinks
    pure nextPatch


findRawLink
    :: forall pstate strepr chrepr mp families
     . Id.LinkR
    -> Patch pstate families strepr chrepr mp
    -> Maybe Raw.Link
findRawLink linkId (Patch _ _ _ _ _ links) =
    links # Links.findRaw linkId


removeNode
    :: forall pstate strepr chrepr mp families
     . Id.NodeR
    -> Patch pstate families strepr chrepr mp
    -> Patch pstate families strepr chrepr mp
removeNode nodeR (Patch name id chState nodes rawNodes links) =
    let
        nextNodes = Map.update (Array.filter (\hnode -> HN.withNode hnode Node.id /= nodeR) >>> Just) (Id.familyOf nodeR) nodes
        nextRawNodes = Map.update (Array.filter (\rnode -> RawNode.id rnode /= nodeR) >>> Just) (Id.familyOf nodeR) rawNodes
    in Patch name id chState nextNodes nextRawNodes links


data MapNodes strepr chrepr m = MapNodes (Map Id.FamilyR (Array (HoldsNode strepr chrepr m)))


instance IsSymbol f => LMap (MapNodes strepr chrepr m) (F f fstate is os chrepr m) (Maybe (Array (HoldsNode strepr chrepr m))) where
    lmap :: MapNodes strepr chrepr m -> Proxy (F f fstate is os chrepr m) -> Maybe (Array (HoldsNode strepr chrepr m))
    lmap (MapNodes families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


class StoresNodesAt :: Type -> Type -> (Type -> Type) -> Families -> Constraint
class    (MapDown (MapNodes strepr chrepr m) families Array (Maybe (Array (HoldsNode strepr chrepr m)))) <= StoresNodesAt strepr chrepr m families
instance (MapDown (MapNodes strepr chrepr m) families Array (Maybe (Array (HoldsNode strepr chrepr m)))) => StoresNodesAt strepr chrepr m families


nonRawNodes
    :: forall pstate families strepr chrepr m
    .  StoresNodesAt strepr chrepr m families
    => Patch pstate families strepr chrepr m
    -> Array (HoldsNode strepr chrepr m)
nonRawNodes (Patch _ _ _ nodes _ _) =
    Array.concat $ Array.catMaybes (mapDown (MapNodes nodes) (Proxy :: _ families) :: Array (Maybe (Array (HoldsNode strepr chrepr m))))


mapNodes
    :: forall x pstate families strepr chrepr m
    .  StoresNodesAt strepr chrepr m families
    => (forall f fstate is os. IsSymbol f => StRepr fstate strepr => Node f fstate is os chrepr m -> x)
    -> Patch pstate families strepr chrepr m
    -> Array x
mapNodes f patch =
    nodeToX <$> nonRawNodes patch
    -- Array.concat $
    --   (map nodeToX)
    --     <$> Array.catMaybes
    --           (mapDown (MapNodes nodes) (Proxy :: _ families) :: Array (Maybe (Array (HoldsNode repr m))))
    where nodeToX hn = HN.withNode hn f


mapRawNodes
    :: forall x pstate families strepr chrepr m
    .  (Raw.Node strepr chrepr m -> x)
    -> Patch pstate families strepr chrepr m
    -> Array x
mapRawNodes f (Patch _ _ _ _ rawNodes _) =
    Array.concat $ Map.toUnfoldable rawNodes <#> Tuple.snd <#> map f


mapAllNodes
    :: forall x pstate families strepr chrepr m
    .  (Raw.Node strepr chrepr m -> x)
    -> Patch pstate families strepr chrepr m
    -> Array x
mapAllNodes f patch@(Patch _ _ _ nodes _ _) =
    Array.concat (map (toRawCnv >>> f) <$> Tuple.snd <$> Map.toUnfoldable nodes) <> mapRawNodes f patch
    where
      toRawCnv hn = HN.withNode hn (Node.toRaw >>> RawNode.toReprableState)


withNodes
    :: forall f fstate is os x pstate families strepr chrepr m
    .  RegisteredFamily (F f fstate is os chrepr m) families
    => IsSymbol f
    => (Node f fstate is os chrepr m -> x)
    -> Id.Family f
    -> Patch pstate families strepr chrepr m
    -> Array x
withNodes f familyId (Patch _ _ _ nodes _ _) =
    Map.lookup (Id.familyR familyId) nodes <#> map nodeToX # fromMaybe []
    where nodeToX hn = HN.withNode hn (unsafeCoerce >>> f)


withRawNodes
    :: forall x pstate families strepr chrepr m
    .  (Raw.Node strepr chrepr m -> x)
    -> Id.FamilyR
    -> Patch pstate families strepr chrepr m
    -> Array x
withRawNodes f familyR (Patch _ _ _ _ rawNodes _) =
    (Map.lookup familyR rawNodes <#> map f) # fromMaybe []


withAnyNodes
    :: forall x pstate families strepr chrepr m
    .  (Raw.Node strepr chrepr m -> x)
    -> Id.FamilyR
    -> Patch pstate families strepr chrepr m
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


subscribeState :: forall pstate families strepr chrepr m. Patch pstate families strepr chrepr m -> Signal pstate
subscribeState (Patch _ _ stchan _ _ _) = Channel.subscribe stchan


getState :: forall pstate families strepr chrepr mi m. MonadEffect m => Patch pstate families strepr chrepr mi -> m pstate
getState = liftEffect <<< Signal.get <<< subscribeState


linksMap :: forall pstate families strepr chrepr m. Patch pstate families strepr chrepr m -> Map Id.LinkR Raw.Link
linksMap (Patch _ _ _ _ _ links) = links


links :: forall pstate families strepr chrepr m. Patch pstate families strepr chrepr m -> Array Raw.Link
links = linksMap >>> Links.all