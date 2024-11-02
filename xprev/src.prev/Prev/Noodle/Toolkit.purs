module Prev.Noodle.Toolkit
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
  , state
  )
  where


import Prelude

import Effect (Effect)
import Effect.Aff (Aff)

import Data.Exists (Exists, mkExists, runExists)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Traversable (sequence)

import Prev.Noodle.Node (Node, NodeFn)
import Prev.Noodle.Node as Node
import Prev.Noodle.Channel as Channel
import Prev.Noodle.Fn (make, name) as Fn
-- import Prev.Noodle.Network (PatchE)
-- import Prev.Noodle.Patch (Patch)

import Unsafe.Coerce (unsafeCoerce)


--type Toolkit d = Toolkit' Unit d


-- FIXME: use Prim.Row types for such cases, instead of `Exists`
data NodeFnS state d fn_state = NodeFnS (NodeFn (state /\ fn_state) d)
type NodeFnE state d = Exists (NodeFnS state d)


type Name = String


-- data Toolkit d = Toolkit d (Node.Family /-> (forall state. NodeFn state d))
data Toolkit patch_state d = Toolkit Name patch_state d (Node.Family /-> NodeFnE patch_state d)


-- make :: forall d. d -> Array (Node.Family /\ Def d) -> Toolkit d
-- make def = Toolkit def <<< Map.fromFoldable


empty :: forall state d. Name -> state -> d -> Toolkit state d
empty name state def = Toolkit name state def $ Map.empty


register
    :: forall patch_state node_state d
     . Toolkit patch_state d
    -> Node.Family
    -> Array (Node.InletId /\ Channel.Def d)
    -> Array (Node.OutletId /\ Channel.Def d)
    -> Node.NodeProcess (patch_state /\ node_state) d
    -> Toolkit patch_state d
register tk family inlets outlets process =
  registerFn tk $ Fn.make family inlets outlets process


registerFn :: forall patch_state node_state d. Toolkit patch_state d -> NodeFn (patch_state /\ node_state) d -> Toolkit patch_state d
registerFn (Toolkit state name def fns) fn =
  Toolkit state name def $ Map.insert (Fn.name fn) (wrapNodeFn fn) $ fns


spawn :: forall patch_state node_state d. Node.Family -> Toolkit patch_state d -> Effect (Maybe (Node (patch_state /\ node_state) d))
spawn family (Toolkit _ _ def nodeDefs) =
    nodeDefs
        # Map.lookup family
        # map (unwrapNodeFn >>> Node.make' def)
        # sequence


spawnAndRun :: forall patch_state node_state d. Node.Family -> node_state -> Toolkit patch_state d -> Effect (Maybe (Node (patch_state /\ node_state) d))
spawnAndRun family node_state tk =
    spawn family tk
      >>= (\maybeNode -> do
            _ <- sequence (Node.run (state tk /\ node_state) <$> maybeNode)
            pure maybeNode
            )


spawnAndRun' :: forall patch_state d. Node.Family -> Toolkit patch_state d -> Effect (Maybe (Node (patch_state /\ Unit) d))
spawnAndRun' family = spawnAndRun family unit


nodeFamilies :: forall patch_state d. Toolkit patch_state d -> Set Node.Family
nodeFamilies (Toolkit _ _ _ nodeDefs) =
    nodeDefs # Map.keys


nodeFamiliesCount :: forall patch_state d. Toolkit patch_state d -> Int
nodeFamiliesCount (Toolkit _ _ _ nodeDefs) =
    nodeDefs # Map.size


name :: forall patch_state d. Toolkit patch_state d -> Name
name (Toolkit name _ _ _) = name


unwrapNodeFn :: forall patch_state node_state d. NodeFnE patch_state d -> NodeFn (patch_state /\ node_state) d
unwrapNodeFn = runExists (\(NodeFnS nodeFn) -> unsafeCoerce nodeFn)


wrapNodeFn :: forall patch_state node_state d. NodeFn (patch_state /\ node_state) d -> NodeFnE patch_state d
wrapNodeFn = mkExists <<< NodeFnS


state :: forall patch_state d. Toolkit patch_state d -> patch_state
state (Toolkit _ state _ _) = state