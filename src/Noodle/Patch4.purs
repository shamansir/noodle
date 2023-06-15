module Noodle.Patch4 where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Array ((:))
import Data.Array as Array
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (sequence)
import Data.Symbol (class IsSymbol)
import Data.SProxy (proxify, reflect)
import Data.SOrder (SOrder)
import Data.SOrder as SOrder

import Record.Unsafe (unsafeGet, unsafeSet, unsafeDelete) as Record
import Unsafe.Coerce (unsafeCoerce)

import Prim.RowList as RL
import Prim.Row (Cons)
import Record as Record
import Record.Extra as Record
import Type.Proxy (Proxy(..))
import Heterogeneous.Mapping as H

import Noodle.Id (Family, Family', NodeId, NodeIdR, HoldsNodeId)
import Noodle.Id as Id
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3.Has as Has
import Noodle.Patch4.Has as Has
import Noodle.Patch4.MapsFolds as PF
import Noodle.Patch4.MapsFolds as PI
import Noodle.Patch4.MapsFolds as PM
import Noodle.Patch4.MapsFolds.Repr as R
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Flatten (NodeLineRec, NodeLineMap) as R
import Noodle.Node2.MapsFolds.Repr (Repr) as R
import Noodle.Patch4.MapsFolds.Repr (class FoldToReprsRec, class FoldToReprsMap)


--data LinkOE fo fi = Exists (LinkOf fo fi)

type Id = String


type Links =
  { from :: Map Node.FromId (forall fo fi i o. Node.Link fo fi i o)
  , to :: Map Node.ToId (forall fo fi i o. Node.Link fo fi i o)
  }


data Patch gstate (instances :: Row Type) = Patch gstate SOrder (Record instances) Links -- FIXME: may be families order is not relevant here


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
        { from : Map.empty
        , to : Map.empty
        }


registerNode
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
registerNode node (Patch state forder instances links) =
    Patch
        state
        forder
        (Record.modify (proxify $ Node.family node) ((:) node) instances) -- NB: notice that Family' f works!
        links


spawnAndRegisterNodeIfKnown
    :: forall gstate instances' instances f families' families state is os m
     . MonadEffect m
    => Has.HasFamilyDef f families' families (Family.Def state is os m)
    => Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Family f
    -> Toolkit gstate families
    -> Patch gstate instances
    -> m (Patch gstate instances)
spawnAndRegisterNodeIfKnown family toolkit patch =
    Toolkit.spawn toolkit family >>= (\node -> pure $ registerNode node patch)


nodesOf
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Family f
    -> Patch ps instances
    -> Array (Node f state is os m)
nodesOf family (Patch _ _ instances _) =
    Record.get (proxify family) instances


howMany
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Family f
    -> Patch ps instances
    -> Int
howMany f = nodesOf f >>> Array.length


registerLink
    :: forall gstate instances fo fi i o
     . Node.Link fo fi i o
    -> Patch gstate instances
    -> Patch gstate instances
registerLink link (Patch state forder instances links) =
  Patch
    state
    forder
    instances
    { from : Map.insert (Node.toFromId link) (unsafeCoerce link) links.from
    , to : Map.insert (Node.toToId link) (unsafeCoerce link) links.to
    }


forgetLink
    :: forall gstate instances fo fi i o
     . Node.Link fo fi i o
    -> Patch gstate instances
    -> Patch gstate instances
forgetLink link (Patch state forder instances links) =
  Patch
    state
    forder
    instances
    { from : Map.delete (Node.toFromId link) links.from
    , to : Map.delete (Node.toToId link) links.to
    }


connect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    => Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    => Id.HasOutput oA doutA osA' osA
    => Id.HasInput iB dinB isB' isB
    => Id.Output oA
    -> Id.Input iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Node.Link fA fB oA iB)
connect o i f na nb patch =
    Node.connect o i f na nb >>= \link -> pure $ registerLink link patch /\ link


-- TODO: unsafeConnect


connectAlike
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    => Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    => Id.HasOutput oA d osA' osA
    => Id.HasInput iB d isB' isB
    => Id.Output oA
    -> Id.Input iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Node.Link fA fB oA iB)
connectAlike o i na nb patch =
    Node.connectAlike o i na nb >>= \link -> pure $ registerLink link patch /\ link


connect'
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    => Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    => Id.HasOutput oA doutA osA' osA
    => Id.HasInput iB dinB isB' isB
    => Id.Output' oA
    -> Id.Input' iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Node.Link fA fB oA iB)
connect' o i f na nb patch =
    Node.connect' o i f na nb >>= \link -> pure $ registerLink link patch /\ link


-- TODO: unsafeConnect


connectAlike'
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    => Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    => Id.HasOutput oA d osA' osA
    => Id.HasInput iB d isB' isB
    => Id.Output' oA
    -> Id.Input' iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Node.Link fA fB oA iB)
connectAlike' o i na nb patch =
    Node.connectAlike' o i na nb >>= \link -> pure $ registerLink link patch /\ link


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' gstate ins insA insB m
     . MonadEffect m
    => MonadRec m
    => Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    => Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    => Id.HasOutput oA doutA osA' osA
    => Id.HasInput iB dinB isB' isB
    => Show dinB
    => Node.Link fA fB oA iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> Patch gstate ins
    -> m (Patch gstate ins /\ Boolean)
disconnect link na nb patch =
    Node.disconnect link na nb >>= \flag -> pure $ forgetLink link patch /\ flag


-- unsafeConnect


newtype HoldsNode m =
    HoldsNode
        (forall r.
            (  forall f gstate instances' instances rli state is os isrl osrl
             . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
            => RL.RowToList instances rli
            => Record.Keys rli
            => Id.HasInputsAt is isrl
            => Id.HasOutputsAt os osrl
            => Patch gstate instances
            -> Node f state is os m
            -> m r
            )
        -> m r)


newtype HoldsNode' gstate instances m =
    HoldsNode'
        (forall r.
            (  forall instances' rli f state is os isrl osrl
             . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
            => RL.RowToList instances rli
            => Record.Keys rli
            => Id.HasInputsAt is isrl
            => Id.HasOutputsAt os osrl
            => Patch gstate instances
            -> Node f state is os m
            -> r
            )
        -> r)


holdNode
    :: forall f gstate instances' instances rli state is os isrl osrl m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => RL.RowToList instances rli
    => Record.Keys rli
    => IsSymbol f
    => Id.HasInputsAt is isrl
    => Id.HasOutputsAt os osrl
    => Patch gstate instances
    -> Node f state is os m
    -> HoldsNode m
holdNode patch node = HoldsNode \f -> f patch node


holdNode'
    :: forall f gstate instances' instances rli state is os isrl osrl m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => RL.RowToList instances rli
    => Record.Keys rli
    => Id.HasInputsAt is isrl
    => Id.HasOutputsAt os osrl
    => Patch gstate instances
    -> Node f state is os m
    -> HoldsNode' gstate instances m
holdNode' patch node = HoldsNode' \f -> f patch node


withNode
    :: forall r m
     . HoldsNode m ->
    --    -> Proxy m ->
        (  forall f gstate instances' instances rli state is os isrl osrl
         . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
        => RL.RowToList instances rli
        => Record.Keys rli
        => Id.HasInputsAt is isrl
        => Id.HasOutputsAt os osrl
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
         . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
        => RL.RowToList instances rli
        => Record.Keys rli
        => Id.HasInputsAt is isrl
        => Id.HasOutputsAt os osrl
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
         . Has.HasInstancesOf fA instancesA' instancesA (Array (Node fA stateA isA osA m))
        => Has.HasInstancesOf fB instancesB' instancesB (Array (Node fB stateB isB osB m))
        => RL.RowToList instancesA rliA
        => Record.Keys rliA
        => RL.RowToList instancesB rliB
        => Record.Keys rliB
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
         . Has.HasInstancesOf fA instancesA' instancesA (Array (Node fA stateA isA osA m))
        => Has.HasInstancesOf fB instancesB' instancesB (Array (Node fB stateB isB osB m))
        => RL.RowToList instancesA rliA
        => Record.Keys rliA
        => RL.RowToList instancesB rliB
        => Record.Keys rliB
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


findNode
    :: forall gstate (instances' :: Row Type) (instances ∷ Row Type) rli f state is os m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
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
