module Noodle.Toolkit
  ( Toolkit
  , empty, name
  , nodeFamilies, nodeFamiliesCount
  , spawn, spawn'
  , register, registerFn
  )
  where


import Prelude ((<<<), (>>>), (#), map, ($), unit, Unit)

import Effect (Effect)
import Effect.Aff (Aff)

import Data.Exists (Exists, mkExists, runExists)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Traversable (sequence)

import Noodle.Node (Node, NodeFn)
import Noodle.Node as Node
import Noodle.Channel as Channel
import Noodle.Fn (make, name) as Fn

import Unsafe.Coerce (unsafeCoerce)


--type Toolkit d = Toolkit' Unit d


data NodeFnS d fn_state = NodeFnS (NodeFn fn_state d)
type NodeFnE d = Exists (NodeFnS d)


type Name = String


-- data Toolkit d = Toolkit d (Node.Family /-> (forall state. NodeFn state d))
data Toolkit d = Toolkit Name d (Node.Family /-> NodeFnE d)



-- make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
-- make def = Toolkit def <<< Map.fromFoldable


empty :: forall state d. Name -> d -> Toolkit d
empty name def = Toolkit name def $ Map.empty


register
    :: forall node_state d
     . Toolkit d
    -> Node.Family
    -> Array (Node.InletId /\ Channel.Def d)
    -> Array (Node.OutletId /\ Channel.Def d)
    -> Node.NodeProcess node_state d
    -> Toolkit d
register tk family inlets outlets process =
  registerFn tk $ Fn.make family inlets outlets process


registerFn :: forall node_state d. Toolkit d -> NodeFn node_state d -> Toolkit d
registerFn (Toolkit name def fns) fn =
  Toolkit name def $ Map.insert (Fn.name fn) (wrapNodeFn fn) $ fns


spawn :: forall d. Node.Family -> Toolkit d -> Effect (Maybe (Node Unit d))
spawn family = spawn' family unit


spawn' :: forall node_state d. Node.Family -> node_state -> Toolkit d -> Effect (Maybe (Node node_state d))
spawn' family state (Toolkit _ def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (unwrapNodeFn >>> Node.make' state def)
        # sequence


-- TODO: spawnAndRun


nodeFamilies :: forall state d. Toolkit d -> Set Node.Family
nodeFamilies (Toolkit _ _ nodeDefs) =
    nodeDefs # Map.keys


nodeFamiliesCount :: forall state d. Toolkit d -> Int
nodeFamiliesCount (Toolkit _ _ nodeDefs) =
    nodeDefs # Map.size


name :: forall d. Toolkit d -> Name
name (Toolkit name _ _) = name


unwrapNodeFn :: forall state d. NodeFnE d -> NodeFn state d
unwrapNodeFn = runExists (\(NodeFnS nodeFn) -> unsafeCoerce nodeFn)


wrapNodeFn :: forall state d. NodeFn state d -> NodeFnE d
wrapNodeFn = mkExists <<< NodeFnS