module Noodle.Patch where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)
import Data.SProxy (proxify)
import Data.SOrder (SOrder)
import Data.Foldable (foldr)
import Data.Newtype (unwrap)

import Prim.RowList as RL
import Record (get, modify) as Record
import Record.Extra (class Keys) as Record
import Type.Proxy (Proxy(..))

import Noodle.Id (Family, Family', LinkId(..), NodeId)
import Noodle.Id as Id
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Toolkit.Has as THas
import Noodle.Patch.Has as PHas
import Noodle.Patch.Is as PIs
import Noodle.Patch.MapsFolds as PF
import Noodle.Patch.MapsFolds as PI
import Noodle.Patch.MapsFolds as PM
import Noodle.Patch.MapsFolds.Repr (class ExtractReprs) as R
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Family.Def as Family
import Noodle.Node.MapsFolds.Flatten (NodeLineMap) as R
import Noodle.Node.MapsFolds.Repr (Repr) as R
import Noodle.Patch.MapsFolds.Repr (class FoldToReprsMap)
import Noodle.Stateful (class Stateful)


--data LinkOE fo fi = Exists (LinkOf fo fi)

type Id = String


type Links =
  { lastId :: LinkId
  , from :: Map Node.FromId HoldsLink
  , to :: Map Node.ToId HoldsLink
  , byNode :: Map Id.NodeIdR (Array (Node.FromId /\ Node.ToId))
  , byId :: Map LinkId HoldsLink
  }


data Patch gstate (instances :: Row Type) = Patch gstate SOrder (Record instances) Links -- FIXME: may be families order is not relevant here


instance Stateful (Patch gstate instances) gstate where
    get :: Patch gstate instances -> gstate
    get (Patch state _ _ _) = state
    set :: gstate -> Patch gstate instances -> Patch gstate instances
    set state (Patch _ order instances links) = Patch state order instances links


init
    :: forall
        (instances ∷ Row Type)
        (families ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . PI.Init rln families instances
    => Toolkit Unit families
    -> Patch Unit instances
init = init' unit


init'
    :: forall
        gstate
        (instances ∷ Row Type)
        (families ∷ Row Type)
        (shapes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . PI.Init rln families instances
    => gstate
    -> Toolkit gstate families
    -> Patch gstate instances
init' state tk =
    Patch
        state
        (Toolkit.familiesOrder tk)
        (PI.init $ Toolkit.toRecord tk)
        { lastId : LinkId $ -1
        , from : Map.empty
        , to : Map.empty
        , byNode : Map.empty
        , byId : Map.empty
        }


registerNode
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
registerNode node (Patch state forder instances links) =
    Patch
        state
        forder
        (Record.modify (proxify $ Node.family node) ((:) node) instances) -- NB: notice that Family' f works!
        links


forgetNode -- TODO: test
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
forgetNode node (Patch state forder instances links) =
    Patch
        state
        forder
        (Record.modify (proxify $ Node.family node) (Array.delete node) instances) -- NB: notice that Family' f works!
        links


-- TODO: forgetLinksOf


spawnAndRegisterNodeIfKnown
    :: forall gstate instances' instances f families' families state is os m
     . MonadEffect m
    => THas.HasFamilyDef f families' families (Family.Def state is os m)
    => PHas.HasNodesOf instances' instances f state is os m
    => Family f
    -> Toolkit gstate families
    -> Patch gstate instances
    -> m (Patch gstate instances)
spawnAndRegisterNodeIfKnown family toolkit patch =
    Toolkit.spawn toolkit family >>= (\node -> pure $ registerNode node patch)


nodesOf
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Family f
    -> Patch ps instances
    -> Array (Node f state is os m)
nodesOf family (Patch _ _ instances _) =
    Record.get (proxify family) instances


howMany
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Family f
    -> Patch ps instances
    -> Int
howMany f = nodesOf f >>> Array.length


registerLink
    :: forall gstate instances fo fi i o
     . IsSymbol fo
    => IsSymbol fi
    => Node.Link fo fi i o
    -> Patch gstate instances
    -> LinkId /\ Patch gstate instances
registerLink link (Patch state forder instances links) =
  nextId /\ Patch
    state
    forder
    instances
    { lastId : nextId
    , from : Map.insert fromId linkHeld links.from
    , to : Map.insert toId linkHeld links.to
    , byNode
        : appendTo (fromId /\ toId) nodeFromIdR
        $ appendTo (fromId /\ toId) nodeToIdR
        $ links.byNode
    , byId :
        Map.insert nextId linkHeld links.byId
    }
   where
    nextId = LinkId $ unwrap links.lastId + 1
    fromId = Node.toFromId link
    toId = Node.toToId link
    linkHeld = holdLink link
    nodeFromIdR = Id.nodeIdR $ Node.fromNode link
    nodeToIdR = Id.nodeIdR $ Node.toNode link
    appendTo value =
        Map.alter $
            case _ of
                Just array -> Just $ value : array
                Nothing -> Just [ value ]


forgetLink
    :: forall gstate instances fo fi i o
     . IsSymbol fo
    => IsSymbol fi
    => Node.Link fo fi i o
    -> Patch gstate instances
    -> Patch gstate instances
forgetLink link (Patch state forder instances links) =
  Patch
    state
    forder
    instances
    { lastId : links.lastId -- TODO: no need in economy of link's IDs I suppose
    , from : Map.delete (Node.toFromId link) links.from
    , to : Map.delete (Node.toToId link) links.to
    , byNode
        : Map.delete (Id.nodeIdR $ Node.fromNode link)
        $ Map.delete (Id.nodeIdR $ Node.toNode link)
        $ links.byNode
    , byId : links.byId -- FIXME: remove link by its id
    }


forgetLink'
    :: forall gstate instances
     . LinkId
    -> Patch gstate instances
    -> Effect Unit /\ Patch gstate instances -- FIXME: cancel the link inside the function, or return HoldsLink
forgetLink' linkId p@(Patch state forder instances links) =
    case Map.lookup linkId links.byId of
        Just linkHeld ->
            withLink linkHeld
                \link ->
                    Node.getCanceler link /\
                    (forgetLink
                        link
                        $ Patch
                            state
                            forder
                            instances
                        $ links
                            { byId = Map.delete linkId links.byId
                            }
                    )
        Nothing -> pure unit /\ p


allLinksOf
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Node f state is os m
    -> Patch ps instances
    -> Array HoldsLink
allLinksOf node (Patch _ _ _ links) =
    case Map.lookup (Id.nodeIdR $ Node.id node) links.byNode of
        Just linksArray ->
            Array.concatMap addLinks $ foldr (:) [] linksArray
        Nothing -> []
    where
        addLinks (fromId /\ toId) =
            case Map.lookup fromId links.from of
                Just fromLink ->
                    case Map.lookup toId links.to of
                        Just toLink -> [ fromLink, toLink ]
                        Nothing -> [ fromLink ]
                Nothing ->
                    case Map.lookup toId links.to of
                        Just toLink -> [ toLink ]
                        Nothing -> []


removeNode -- TODO: test
    :: forall ps instances' instances f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
removeNode node =
    forgetNode node
    >>> \patch -> foldr forget patch $ allLinksOf node patch
    where
        forget holdsLink = withLink holdsLink forgetLink


connect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => PIs.LinkStartInPatch fA oA doutA stateA isA osA osA' gstate ins insA m
    => PIs.LinkEndInPatch fB iB dinB stateB isB isB' osB gstate ins insB m
    => Id.Output oA
    -> Id.Input iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m ((LinkId /\ Patch gstate ins) /\ Node.Link fA fB oA iB)
connect o i f na nb patch =
    Node.connect o i f na nb >>= \link -> pure $ registerLink link patch /\ link


-- TODO: unsafeConnect


connectAlike
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => PIs.LinkStartInPatch fA oA d stateA isA osA osA' gstate ins insA m
    => PIs.LinkEndInPatch fB iB d stateB isB isB' osB gstate ins insB m
    => Id.Output oA
    -> Id.Input iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m ((LinkId /\ Patch gstate ins) /\ Node.Link fA fB oA iB)
connectAlike o i na nb patch =
    Node.connectAlike o i na nb >>= \link -> pure $ registerLink link patch /\ link


connect'
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => PIs.LinkStartInPatch fA oA doutA stateA isA osA osA' gstate ins insA m
    => PIs.LinkEndInPatch fB iB dinB stateB isB isB' osB gstate ins insB m
    => Id.Output' oA
    -> Id.Input' iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m ((LinkId /\ Patch gstate ins) /\ Node.Link fA fB oA iB)
connect' o i f na nb patch =
    Node.connect' o i f na nb >>= \link -> pure $ registerLink link patch /\ link


-- TODO: unsafeConnect


connectAlike'
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => PIs.LinkStartInPatch fA oA d stateA isA osA osA' gstate ins insA m
    => PIs.LinkEndInPatch fB iB d stateB isB isB' osB gstate ins insB m
    => Id.Output' oA
    -> Id.Input' iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m ((LinkId /\ Patch gstate ins) /\ Node.Link fA fB oA iB)
connectAlike' o i na nb patch =
    Node.connectAlike' o i na nb >>= \link -> pure $ registerLink link patch /\ link


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => PIs.LinkStartInPatch fA oA doutA stateA isA osA osA' gstate ins insA m
    => PIs.LinkEndInPatch fB iB dinB stateB isB isB' osB gstate ins insB m
    => Show dinB
    => Node.Link fA fB oA iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Boolean)
disconnect link na nb patch =
    Node.disconnect link na nb >>= \flag -> pure $ forgetLink link patch /\ flag


newtype HoldsNode m =
    HoldsNode
        (forall r.
            (  forall f gstate instances' instances rli state is os isrl osrl
             . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
            => Patch gstate instances
            -> Node f state is os m
            -> m r
            )
        -> m r)


newtype HoldsNode' gstate instances m =
    HoldsNode'
        (forall r.
            (  forall instances' rli f state is os isrl osrl
             . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
            => Patch gstate instances
            -> Node f state is os m
            -> r
            )
        -> r)


newtype HoldsNodeMRepr (x :: Symbol -> Type) gstate instances m repr =
    HoldsNodeMRepr
        (forall r.
            (  forall instances' rli f state is os isrl osrl repr_is repr_os
             . PIs.IsNodeInPatch' x gstate instances instances' rli f state is os isrl osrl repr_is repr_os repr m
            => Patch gstate instances
            -> Node f state is os m
            -> r
            )
        -> r)


holdNode
    :: forall f gstate instances' instances rli state is os isrl osrl m
     . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
    => Patch gstate instances
    -> Node f state is os m
    -> HoldsNode m
holdNode patch node = HoldsNode \f -> f patch node


holdNode'
    :: forall f gstate instances' instances rli state is os isrl osrl m
     . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
    => Patch gstate instances
    -> Node f state is os m
    -> HoldsNode' gstate instances m
holdNode' patch node = HoldsNode' \f -> f patch node


holdNodeMRepr
    :: forall x gstate instances m repr instances' rli f state is os isrl osrl repr_is repr_os
     . PIs.IsNodeInPatch' x gstate instances instances' rli f state is os isrl osrl repr_is repr_os repr m
    => Patch gstate instances
    -> Node f state is os m
    -> HoldsNodeMRepr x gstate instances m repr
holdNodeMRepr patch node = HoldsNodeMRepr \f -> f patch node


-- Has.HasNodesOf instances' instances f state is os m
--     , RL.RowToList instances rli
--     , Record.Keys rli
--     , Id.HasInputsAt is isrl
--     , Id.HasOutputsAt os osrl



withNode
    :: forall r m
     . HoldsNode m ->
    --    -> Proxy m ->
        (  forall f gstate instances' instances rli state is os isrl osrl
         . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
        => Patch gstate instances
        -> Node f state is os m
        -> m r
        )
    -> m r
withNode (HoldsNode f) = f


withNode'
    :: forall gstate instances m r
     . HoldsNode' gstate instances m ->
        (  forall instances' rli f state is os isrl osrl
         . PIs.IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m
        => Patch gstate instances
        -> Node f state is os m
        -> r
        )
    -> r
withNode' (HoldsNode' f) = f


withNode2
    :: forall r m
     . HoldsNode m
    -> HoldsNode m
    ->
    --    -> Proxy m ->
        (  forall
                  fA stateA isA osA
                  fB stateB isB osB
                  gstateA instancesA' instancesA rliA
                  gstateB instancesB' instancesB rliB
                  isrlA osrlA isrlB osrlB
         . PIs.IsNodeInPatch gstateA instancesA instancesA' rliA fA stateA isA osA isrlA osrlA m
        => PIs.IsNodeInPatch gstateB instancesB instancesB' rliB fB stateB isB osB isrlB osrlB m
        => Node fA stateA isA osA m
        -> Node fB stateB isB osB m
        -> Patch gstateA instancesA
        -> Patch gstateB instancesB
        -> m r
        )
    -> m r
withNode2 (HoldsNode fA) (HoldsNode fB) f =
    fA
        (\patchA nodeA ->
            fB
                (\patchB nodeB ->
                    f nodeA nodeB patchA patchB
                )
        )


withNode2'
    :: forall r m
            gstateA instancesA
            gstateB instancesB
     . HoldsNode' gstateA instancesA m
    -> HoldsNode' gstateB instancesB m
    ->
    --    -> Proxy m ->
        (  forall
                  fA stateA isA osA
                  fB stateB isB osB
                  instancesA' rliA
                  instancesB' rliB
                  isrlA osrlA isrlB osrlB
         . PIs.IsNodeInPatch gstateA instancesA instancesA' rliA fA stateA isA osA isrlA osrlA m
        => PIs.IsNodeInPatch gstateB instancesB instancesB' rliB fB stateB isB osB isrlB osrlB m
        => Node fA stateA isA osA m
        -> Node fB stateB isB osB m
        -> Patch gstateA instancesA
        -> Patch gstateB instancesB
        -> m r
        )
    -> m r
withNode2' (HoldsNode' fA) (HoldsNode' fB) f =
    fA
        (\patchA nodeA ->
            fB
                (\patchB nodeB ->
                    f nodeA nodeB patchA patchB
                )
        )


withNodeMRepr
    :: forall r (x :: Symbol -> Type) gstate instances repr m
     . HoldsNodeMRepr x gstate instances m repr ->
    --    -> Proxy m ->
        (  forall instances' rli f state is os isrl osrl repr_is repr_os
        .  PIs.IsNodeInPatch' x gstate instances instances' rli f state is os isrl osrl repr_is repr_os repr m
        => Patch gstate instances
        -> Node f state is os m
        -> m r
        )
    -> m r
withNodeMRepr (HoldsNodeMRepr f) = f


withNode2MRepr
    :: forall x r m repr
            gstateA instancesA
            gstateB instancesB
     . HoldsNodeMRepr x gstateA instancesA m repr
    -> HoldsNodeMRepr x gstateB instancesB m repr
    ->
    --    -> Proxy m ->
        (  forall
                  fA stateA isA isrlA osA osrlA
                  fB stateB isB isrlB osB osrlB
                  repr_isA repr_osA
                  repr_isB repr_osB
                  instancesA' rliA
                  instancesB' rliB
         . PIs.IsNodeInPatch' x gstateA instancesA instancesA' rliA fA stateA isA osA isrlA osrlA repr_isA repr_osA repr m
        => PIs.IsNodeInPatch' x gstateB instancesB instancesB' rliB fB stateB isB osB isrlB osrlB repr_isB repr_osB repr m
        => Node fA stateA isA osA m
        -> Node fB stateB isB osB m
        -> Patch gstateA instancesA
        -> Patch gstateB instancesB
        -> m r
        )
    -> m r
withNode2MRepr (HoldsNodeMRepr fA) (HoldsNodeMRepr fB) f =
    fA
        (\patchA nodeA ->
            fB
                (\patchB nodeB ->
                    f nodeA nodeB patchA patchB
                )
        )


findNode
    :: forall gstate (instances' :: Row Type) (instances ∷ Row Type) rli f state is os m
     . PHas.HasNodesOf instances' instances f state is os m
    => RL.RowToList instances rli
    => Record.Keys rli
    => NodeId f
    -> Patch gstate instances
    -> Maybe (Node f state is os m)
findNode nodeId (Patch _ _ instances _) =
    let
        (family :: Family' f) = Id.familyOf nodeId
        (familiesArray :: Array (Node f state is os m)) = Record.get (proxify family) instances
    in
        Array.find (\otherNode -> Node.id otherNode == nodeId) familiesArray


-- TODO: some generic existential type
{-
withNode' :: forall gstate (instances' :: Row Type) (instances ∷ Row Type) rli m a. RL.RowToList instances rli => Record.Keys rli => Applicative m => (forall f state is os. Node f state is os m -> m a) -> NodeIdR -> Patch gstate instances -> m (Maybe a)
withNode' fn nodeId (Patch _ _ instances _) =
    let
        familyR /\ hash = Id.splitR nodeId
        familyStr = Id.reflect' familyR
    in
        if List.elem (Id.reflect' familyR) $ Record.keys instances then
            let
                familyNodeArray = Record.unsafeGet familyStr instances
                maybeNode = Array.find (\node -> Id.hashOf (Node.id node) == Id.hashOfR nodeId) familyNodeArray
            in case maybeNode of
                    Just node ->
                        sequence $ Just $ fn node
                    Nothing ->
                        pure Nothing
        else
            pure Nothing -}


nodes_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . PF.Fold rla instances ff result
    => Patch gstate instances
    -> ff result
nodes_ (Patch _ _ instances _) =
    PF.hfoldl_ instances


nodes
    :: forall f state is os gstate instances rla m
     . PF.Fold rla instances Array (Node f state is os m)
    => Patch gstate instances
    -> Array (Node f state is os m)
nodes (Patch _ forder instances _) =
    -- SOrder.sortBy' forder (reflect <<< Node.family)
    -- $ PF.hfoldl instances
    PF.hfoldl instances


nodesIndexed_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . PF.FoldI rla instances ff result
    => Patch gstate instances
    -> ff result
nodesIndexed_ (Patch _ _ instances _) =
    PF.hfoldlWithIndex_ instances


nodesIndexed
    :: forall f state is os gstate (instances :: Row Type) (rla ∷ RL.RowList Type) (m :: Type -> Type)
     . PF.FoldI rla instances Array (PF.NodeWithIndex f state is os m)
    => Patch gstate instances
    -> Array (PF.NodeWithIndex f state is os m)
nodesIndexed (Patch _ _ instances _) =
     PF.hfoldlWithIndex instances


nodesMap
    :: forall gstate instances rli x instances'
     . PM.Map rli instances instances' x
    => Patch gstate instances
    -> Record instances'
nodesMap (Patch _ _ instances _) =
    PM.hmap (Proxy :: Proxy x) instances


nodesMapIndexed
    :: forall gstate instances rli x instances'
     . PM.MapI rli instances instances' x
    => Patch gstate instances
    -> Record instances'
nodesMapIndexed (Patch _ _ instances _) =
    PM.hmapWithIndex (Proxy :: Proxy x) instances


toRepr
    :: forall m gstate (instances :: Row Type) (rla ∷ RL.RowList Type) (reprs :: Row Type) repr
     . MonadEffect m
    => R.ExtractReprs m rla instances reprs repr
    => Proxy m
    -> R.Repr repr
    -> Patch gstate instances
    -> Record reprs
toRepr mproxy repr (Patch _ _ instances _) =
    PF.toReprs mproxy repr instances


toReprFlat
    :: forall gstate m (instances :: Row Type) (rla ∷ RL.RowList Type) repr
     . MonadEffect m
    => FoldToReprsMap m rla instances repr
    => Proxy m
    -> R.Repr repr
    -> Patch gstate instances
    -> m (Array (R.NodeLineMap repr))
toReprFlat mproxy repr (Patch _ _ instances _) =
    PF.toReprsFlat mproxy repr instances



newtype HoldsLink = HoldsLink (forall r. (forall fo fi i o. IsSymbol fo => IsSymbol fi => Node.Link fo fi i o -> r) -> r)


holdLink :: forall fo fi i o. IsSymbol fo => IsSymbol fi => Node.Link fo fi i o -> HoldsLink
holdLink link = HoldsLink (_ $ link)


withLink :: forall r. HoldsLink -> (forall fo fi i o. IsSymbol fo => IsSymbol fi => Node.Link fo fi i o -> r) -> r
withLink (HoldsLink f) = f
