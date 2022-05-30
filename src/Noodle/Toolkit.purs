module Noodle.Toolkit
  ( Toolkit
  , empty
  , name
  , nodeFamilies
  , nodeFamiliesCount
  , register
  , registerFn
  , spawn
  , spawnAndRun
  , spawnAndRun'
  )
  where


import Prelude

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
-- import Noodle.Network (PatchE)
-- import Noodle.Patch (Patch)

import Unsafe.Coerce (unsafeCoerce)


--type Toolkit d = Toolkit' Unit d


data NodeFnS d fn_state = NodeFnS (NodeFn fn_state d)
type NodeFnE d = Exists (NodeFnS d)


type Name = String


-- data Toolkit d = Toolkit d (Node.Family /-> (forall state. NodeFn state d))
data Toolkit patch_state d = Toolkit Name patch_state d (Node.Family /-> NodeFnE d)


-- make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
-- make def = Toolkit def <<< Map.fromFoldable


empty :: forall state d. Name -> state -> d -> Toolkit state d
empty name state def = Toolkit name state def $ Map.empty


register
    :: forall state node_state d
     . Toolkit state d
    -> Node.Family
    -> Array (Node.InletId /\ Channel.Def d)
    -> Array (Node.OutletId /\ Channel.Def d)
    -> Node.NodeProcess node_state d
    -> Toolkit state d
register tk family inlets outlets process =
  registerFn tk $ Fn.make family inlets outlets process


registerFn :: forall patch_state node_state d. Toolkit patch_state d -> NodeFn node_state d -> Toolkit patch_state d
registerFn (Toolkit state name def fns) fn =
  Toolkit state name def $ Map.insert (Fn.name fn) (wrapNodeFn fn) $ fns


spawn :: forall state node_state d. Node.Family -> Toolkit state d -> Effect (Maybe (Node node_state d))
spawn family (Toolkit _ _ def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (unwrapNodeFn >>> Node.make' def)
        # sequence


spawnAndRun :: forall state node_state d. Node.Family -> node_state -> Toolkit state d -> Effect (Maybe (Node node_state d))
spawnAndRun family state tk =
    spawn family tk
      >>= (\maybeNode -> do
            _ <- sequence (Node.run state <$> maybeNode)
            pure maybeNode
            )


spawnAndRun' :: forall state d. Node.Family -> Toolkit state d -> Effect (Maybe (Node Unit d))
spawnAndRun' family = spawnAndRun family unit


nodeFamilies :: forall state d. Toolkit state d -> Set Node.Family
nodeFamilies (Toolkit _ _ _ nodeDefs) =
    nodeDefs # Map.keys


nodeFamiliesCount :: forall state d. Toolkit state d -> Int
nodeFamiliesCount (Toolkit _ _ _ nodeDefs) =
    nodeDefs # Map.size


name :: forall state d. Toolkit state d -> Name
name (Toolkit name _ _ _) = name


unwrapNodeFn :: forall state d. NodeFnE d -> NodeFn state d
unwrapNodeFn = runExists (\(NodeFnS nodeFn) -> unsafeCoerce nodeFn)


wrapNodeFn :: forall state d. NodeFn state d -> NodeFnE d
wrapNodeFn = mkExists <<< NodeFnS