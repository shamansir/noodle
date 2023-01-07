module Noodle.Patch4 where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Noodle.Id (Family)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Patch4.Has as Has
import Noodle.Patch4.MapsFolds as PF
import Noodle.Patch4.MapsFolds as PI
import Noodle.Patch4.MapsFolds as PM
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Prim.RowList as RL
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy (Proxy(..))
import Heterogeneous.Mapping as H

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
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . PI.Init rln nodes instances
    => Toolkit Unit nodes
    -> Patch Unit instances
init = init' unit


init'
    :: forall
        gstate
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . PI.Init rln nodes instances
    => gstate
    -> Toolkit gstate nodes
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
     . Has.HasInstancesOf f instances' instances (PM.NodesOf f state is os m)
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
registerNode node (Patch state instances links) =
    Patch
        state
        (Record.modify (Node.family node) ((:) node) instances) -- NB: notice that Family' f works!
        links


nodesOf
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (PM.NodesOf f state is os m)
    => Family f
    -> Patch ps instances
    -> PM.NodesOf f state is os m
nodesOf family (Patch _ instances _) = Record.get family instances


howMany
    :: forall ps instances' instances f state is os m
     . Has.HasInstancesOf f instances' instances (PM.NodesOf f state is os m)
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
     . PF.Fold rla ff result instances
    => Patch gstate instances
    -> ff result
nodes_ (Patch _ instances _) =
    PF.hfoldl_ instances


nodes
    :: forall f state is os gstate instances rla m
     . PF.Fold rla Array (Node f state is os m) instances
    => Patch gstate instances
    -> Array (Node f state is os m)
nodes (Patch _ instances _) =
    PF.hfoldl instances


nodesIndexed_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . PF.FoldI rla ff result instances
    => Patch gstate instances
    -> ff result
nodesIndexed_ (Patch _ instances _) =
    PF.hfoldlWithIndex_ instances


nodesIndexed
    :: forall f state is os gstate (instances :: Row Type) (rla ∷ RL.RowList Type) (m :: Type -> Type)
     . PF.FoldI rla Array (PF.NodeWithIndex f state is os m) instances
    => Patch gstate instances
    -> Array (PF.NodeWithIndex f state is os m)
nodesIndexed (Patch _ instances _) =
     PF.hfoldlWithIndex instances


nodesMap
    :: forall gstate instances rli x instances'
     . PM.Map rli instances x instances'
    => Patch gstate instances
    -> Record instances'
nodesMap (Patch _ instances _) =
    PM.hmap (Proxy :: Proxy x) instances


nodesMapIndexed
    :: forall gstate instances rli x instances'
     . PM.MapI rli instances x instances'
    => Patch gstate instances
    -> Record instances'
nodesMapIndexed (Patch _ instances _) =
    PM.hmapWithIndex (Proxy :: Proxy x) instances
