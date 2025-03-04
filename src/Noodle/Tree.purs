module Noodle.Tree where

import Prelude

import Noodle.Id as Id

import Effect.Class (class MonadEffect)

import Data.Map (values) as Map
import Data.List (toUnfoldable) as List
import Data.Tuple (uncurry) as Tuple
import Data.Traversable (sequence, class Traversable)

import Noodle.Network (Network)
import Noodle.Network (patches) as Network
import Noodle.Patch (Patch)
import Noodle.Patch (mapAllNodes, allRawLinks) as Patch
import Noodle.Raw.Fn.Shape (InletDefR, OutletDefR) as Raw
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (inlets', outlets') as RawNode
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Yoga.Tree (Tree)
import Yoga.Tree as Tree


data TreeNode pstate fs strepr chrepr m a
    = R a -- (Network tk pstate families strepr chrepr m)
    | P (Patch pstate fs strepr chrepr m) a
    | L Raw.Link a
    | N (Raw.Node strepr chrepr m) a
    | I Raw.InletDefR (ValueInChannel chrepr) a
    | O Raw.OutletDefR (ValueInChannel chrepr) a


derive instance Functor (TreeNode pstate fs strepr chrepr m)


data PathTreeNode repr a
    = Root a
    -- | Toolkit Id.ToolkitR a
    -- | Family Id.FamilyR a
    | Patch Id.PatchR a
    | Link Id.PatchR Id.Link a
    | Node Id.PatchR Id.NodeR a
    | Inlet Id.PatchR Id.NodeR Id.InletR (ValueInChannel repr) a
    | Outlet Id.PatchR Id.NodeR Id.OutletR (ValueInChannel repr) a


toTree_
    :: forall m tk pstate families strepr chrepr mp a
     . MonadEffect m
    => (TreeNode pstate families strepr chrepr mp Unit -> a)
    -> Network tk pstate families strepr chrepr mp
    -> m (Tree (TreeNode pstate families strepr chrepr mp a))
toTree_ makeVal network = do
    sequenceH (Tree.mkTree $ withVal R) $ patchTree <$> (List.toUnfoldable $ Map.values $ Network.patches network)
    where
        sequenceH :: forall t x v. Traversable t => Applicative m => (t v -> x) -> t (m v) -> m x
        sequenceH f leaves = sequence leaves <#> \leavesx -> f leavesx
        patchTree :: Patch pstate families strepr chrepr mp -> m (Tree (TreeNode pstate families strepr chrepr mp a))
        patchTree patch = sequenceH (Tree.mkTree $ withVal (P patch)) $ Patch.mapAllNodes nodeTree patch <> (linkLeaf <$> Patch.allRawLinks patch)
        nodeTree :: Raw.Node strepr chrepr mp -> m (Tree (TreeNode pstate families strepr chrepr mp a))
        nodeTree node = sequenceH (Tree.mkTree (withVal (N node))) $ (Tuple.uncurry inletLeaf <$> RawNode.inlets' node) <> (Tuple.uncurry outletLeaf <$> RawNode.outlets' node)
        linkLeaf :: Raw.Link -> m (Tree (TreeNode pstate families strepr chrepr mp a))
        linkLeaf link = pure $ Tree.leaf $ withVal (L link)
        inletLeaf :: Raw.InletDefR -> m (ValueInChannel chrepr) -> m (Tree (TreeNode pstate families strepr chrepr mp a))
        inletLeaf inletDef mValue = mValue <#> \vicVal -> Tree.leaf $ withVal (I inletDef vicVal)
        outletLeaf :: Raw.OutletDefR -> m (ValueInChannel chrepr) -> m (Tree (TreeNode pstate families strepr chrepr mp a))
        outletLeaf outletDef mValue = mValue <#> \vicVal -> Tree.leaf $ withVal (O outletDef vicVal)
        withVal :: (forall x. x -> TreeNode pstate families strepr chrepr mp x) -> (TreeNode pstate families strepr chrepr mp a)
        withVal f = f $ makeVal $ f unit