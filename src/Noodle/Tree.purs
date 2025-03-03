module Noodle.Tree where

import Prelude

import Noodle.Id as Id

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe (..))
import Data.Map (values) as Map
import Data.List (toUnfoldable) as List

import Noodle.Network (Network)
import Noodle.Network (patches) as Network
import Noodle.Patch (Patch)
import Noodle.Patch (mapAllNodes, allRawLinks) as Patch
import Noodle.Raw.Fn.Shape (InletDefR, OutletDefR) as Raw
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Node (Node) as Raw
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Yoga.Tree (Tree(..))
import Yoga.Tree as Tree


data TreeNode m pstate fs strepr chrepr mp a
    = R a -- (Network tk pstate families strepr chrepr m)
    | P (Patch pstate fs strepr chrepr mp) a
    | L Raw.Link a
    | N (Raw.Node strepr chrepr mp) a
    | I Id.InletR Raw.InletDefR (m (ValueInChannel chrepr)) a
    | O Id.OutletR Raw.OutletDefR (m (ValueInChannel chrepr)) a


derive instance Functor (TreeNode pstate fs strepr chrepr mi mo)


data PathTreeNode repr a
    = Root a
    -- | Toolkit Id.ToolkitR a
    -- | Family Id.FamilyR a
    | Patch Id.PatchR a
    | Link Id.PatchR Id.Link a
    | Node Id.PatchR Id.NodeR a
    | Inlet Id.PatchR Id.NodeR Id.InletR (ValueInChannel repr) a
    | Outlet Id.PatchR Id.NodeR Id.OutletR (ValueInChannel repr) a


toTree
    :: forall m tk pstate families strepr chrepr mp a
     . MonadEffect m
    => (TreeNode m pstate families strepr chrepr mp Unit -> a)
    -> Network tk pstate families strepr chrepr mp
    -> Tree (TreeNode m pstate families strepr chrepr mp a)
toTree makeVal network =
    Tree.mkTree (withVal R) $ patchTree <$> (List.toUnfoldable $ Map.values $ Network.patches network)
    where
        patchTree :: Patch pstate families strepr chrepr mp -> Tree (TreeNode m pstate families strepr chrepr mp a)
        patchTree patch = Tree.mkTree (withVal (P patch)) $ Patch.mapAllNodes nodeTree patch <> (linkLeaf <$> Patch.allRawLinks patch)
        nodeTree :: Raw.Node strepr chrepr mp -> Tree (TreeNode m pstate families strepr chrepr mp a)
        nodeTree node = Tree.mkTree (withVal (N node)) []
        linkLeaf :: Raw.Link -> Tree (TreeNode m pstate families strepr chrepr mp a)
        linkLeaf link = Tree.leaf $ withVal (L link)
        inletLeaf :: Id.InletR -> Raw.InletDefR -> m (ValueInChannel chrepr) -> Tree (TreeNode m pstate families strepr chrepr mp a)
        inletLeaf inletR inletDef mValue = Tree.leaf $ withVal (I inletR inletDef mValue)
        outletLeaf :: Id.OutletR -> Raw.OutletDefR -> m (ValueInChannel chrepr) -> Tree (TreeNode m pstate families strepr chrepr mp a)
        outletLeaf outletR outletDef mValue = Tree.leaf $ withVal (O outletR outletDef mValue)
        withVal :: (forall x. x -> TreeNode m pstate families strepr chrepr mp x) -> (TreeNode m pstate families strepr chrepr mp a)
        withVal f = f $ makeVal $ f unit