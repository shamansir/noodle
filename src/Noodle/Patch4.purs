module Noodle.Patch4 where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map

import Unsafe.Coerce (unsafeCoerce)

import Prim.RowList as RL
import Prim.Row (Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import Heterogeneous.Mapping as H

import Noodle.Id (Family)
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
import Noodle.Patch4.MapsFolds.Repr (Repr, class FoldToReprsRec, class FoldToReprsMap, NodeLineRec, NodeLineMap)


--data LinkOE fo fi = Exists (LinkOf fo fi)

type Id = String


type Links =
  { from :: Map Node.FromId (forall fo fi i o. Node.Link fo fi i o)
  , to :: Map Node.ToId (forall fo fi i o. Node.Link fo fi i o)
  }


data Patch gstate (instances :: Row Type) = Patch gstate (Record instances) Links


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
registerNode node (Patch state instances links) =
    Patch
        state
        (Record.modify (Node.family node) ((:) node) instances) -- NB: notice that Family' f works!
        links


spawnAdd
    :: forall gstate instances' instances f families' families state is os m
     . MonadEffect m
    => Has.HasFamilyDef f families' families (Family.Def state is os m)
    => Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Family f
    -> Toolkit gstate families
    -> Patch gstate instances
    -> m (Patch gstate instances)
spawnAdd family toolkit patch =
    Toolkit.spawn toolkit family >>= (\node -> pure $ registerNode node patch)


nodesOf
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    => Family f
    -> Patch ps instances
    -> Array (Node f state is os m)
nodesOf family (Patch _ instances _) = Record.get family instances


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
registerLink link (Patch state instances links) =
  Patch
    state
    instances
    { from : Map.insert (Node.toFromId link) (unsafeCoerce link) links.from
    , to : Map.insert (Node.toToId link) (unsafeCoerce link) links.to
    }


nodes_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . PF.Fold rla instances ff result
    => Patch gstate instances
    -> ff result
nodes_ (Patch _ instances _) =
    PF.hfoldl_ instances


nodes
    :: forall f state is os gstate instances rla m
     . PF.Fold rla instances Array (Node f state is os m)
    => Patch gstate instances
    -> Array (Node f state is os m)
nodes (Patch _ instances _) =
    PF.hfoldl instances


nodesIndexed_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . PF.FoldI rla instances ff result
    => Patch gstate instances
    -> ff result
nodesIndexed_ (Patch _ instances _) =
    PF.hfoldlWithIndex_ instances


nodesIndexed
    :: forall f state is os gstate (instances :: Row Type) (rla ∷ RL.RowList Type) (m :: Type -> Type)
     . PF.FoldI rla instances Array (PF.NodeWithIndex f state is os m)
    => Patch gstate instances
    -> Array (PF.NodeWithIndex f state is os m)
nodesIndexed (Patch _ instances _) =
     PF.hfoldlWithIndex instances


nodesMap
    :: forall gstate instances rli x instances'
     . PM.Map rli instances instances' x
    => Patch gstate instances
    -> Record instances'
nodesMap (Patch _ instances _) =
    PM.hmap (Proxy :: Proxy x) instances


nodesMapIndexed
    :: forall gstate instances rli x instances'
     . PM.MapI rli instances instances' x
    => Patch gstate instances
    -> Record instances'
nodesMapIndexed (Patch _ instances _) =
    PM.hmapWithIndex (Proxy :: Proxy x) instances


toRepr
    :: forall m gstate (instances :: Row Type) (rla ∷ RL.RowList Type) (reprs :: Row Type) repr
     . MonadEffect m
    => R.ExtractReprs m rla instances reprs repr
    => Proxy m
    -> R.Repr repr
    -> Patch gstate instances
    -> Record reprs
toRepr mproxy repr (Patch _ instances _) =
    PF.toReprs mproxy repr instances


{-
toRepr'
    :: forall f gstate m (instances :: Row Type) (rla ∷ RL.RowList Type) (ff :: Type -> Type) repr
     . MonadEffect m
    => FoldToReprsMap m rla instances f repr
    => Repr repr
    -> Patch gstate instances
    -> m (Array (NodeLineMap f repr))
toRepr' repr (Patch _ instances _) =
    PF.toReprs' repr instances
-}
