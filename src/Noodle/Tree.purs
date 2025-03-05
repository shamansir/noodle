module Noodle.Tree where

import Prelude

import Noodle.Id as Id

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe, maybe)
import Data.Map (values) as Map
import Data.List (toUnfoldable) as List
import Data.Tuple (uncurry, fst, snd) as Tuple
import Data.Traversable (sequence, class Traversable)
import Data.Foldable (foldl)
import Data.Newtype as NT
import Data.UniqueHash (toString) as UH

import Data.Text.Format as T

import Noodle.Id (hashOf) as Id
import Noodle.Network (Network)
import Noodle.Network (patches) as Network
import Noodle.Patch (Patch)
import Noodle.Patch (id, mapAllNodes, allRawLinks) as Patch
import Noodle.Raw.Fn.Shape (InletDefR, OutletDefR) as Raw
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id, Connector, connector, fromNode, toNode, fromOutlet, toInlet) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, inlets', outlets') as RawNode
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Noodle.Ui.Cli.Tagging as T

import Yoga.Tree (Tree)
import Yoga.Tree as Tree


newtype NetworkTree pstate families strepr chrepr mp a = NetworkTree (Tree (TreeNode pstate families strepr chrepr mp a))
newtype NetworkPathTree chrepr a = NetworkPathTree (Tree (PathTreeNode chrepr a))


derive instance NT.Newtype (NetworkTree pstate families strepr chrepr mp a) _
derive instance NT.Newtype (NetworkPathTree chrepr a) _


data TreeNode pstate fs strepr chrepr m a
    = R a -- (Network tk pstate families strepr chrepr m)
    | P (Patch pstate fs strepr chrepr m) a
    | L Raw.Link a
    | N (Raw.Node strepr chrepr m) a
    | I Raw.InletDefR (ValueInChannel chrepr) a
    | O Raw.OutletDefR (ValueInChannel chrepr) a


derive instance Functor (TreeNode pstate fs strepr chrepr m)
derive instance Functor (PathTreeNode chrepr)
derive instance Functor (NetworkTree pstate fs strepr chrepr m)
derive instance Functor (NetworkPathTree chrepr)


data PathTreeNode repr a
    = Root a
    -- | Toolkit Id.ToolkitR a
    -- | Family Id.FamilyR a
    | Patch Id.PatchR a
    | Link (Maybe Id.Link) RawLink.Connector a
    | Node Id.NodeR a
    | Inlet Raw.InletDefR (ValueInChannel repr) a
    | Outlet Raw.OutletDefR (ValueInChannel repr) a


toTree
    :: forall m tk pstate families strepr chrepr mp a
     . MonadEffect m
    => (TreeNode pstate families strepr chrepr mp Unit -> a)
    -> Network tk pstate families strepr chrepr mp
    -> m (NetworkTree pstate families strepr chrepr mp a)
toTree makeVal network = do
    NT.wrap <$> (sequenceH (Tree.mkTree $ withVal R) $ patchTree <$> (List.toUnfoldable $ Map.values $ Network.patches network))
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


toPathTree
    :: forall m tk pstate families strepr chrepr mp a
     . MonadEffect m
    => (PathTreeNode chrepr Unit -> a)
    -> Network tk pstate families strepr chrepr mp
    -> m (NetworkPathTree chrepr a)
toPathTree makeVal =
    toTree (toPathNode >>> makeVal) <#> map extractPaths


toPathNode :: forall pstate families strepr chrepr mp a. TreeNode pstate families strepr chrepr mp a -> PathTreeNode chrepr a
toPathNode = case _ of
    R a -> Root a
    P patch a -> Patch (Patch.id patch) a
    L link a -> Link (RawLink.id link) (RawLink.connector link) a
    N node a -> Node (RawNode.id node) a
    I def vicval a -> Inlet def vicval a
    O def vicval a -> Outlet def vicval a


extractPaths :: forall pstate families strepr chrepr mp a. NetworkTree pstate families strepr chrepr mp a -> NetworkPathTree chrepr a
extractPaths = NT.unwrap >>> map toPathNode >>> NT.wrap


formatPathTree :: forall chrepr a. NetworkPathTree chrepr a -> T.Tag
formatPathTree = NT.unwrap >>> map stringifyNode >>> Tree.showTree >>> T.s
    where
        stringifyNode = case _ of
            Root a -> ""
            Patch patchId a -> "P " <> (UH.toString $ Id.hashOf patchId)
            Link mbId conn a ->
                "L " <> maybe "-" show mbId <> " "
                    <> show (Tuple.fst $ _.from conn) <> " " <> show (Tuple.snd $ _.from conn) <> " "
                    <> show (Tuple.fst $ _.to conn) <> " " <> show (Tuple.snd $ _.to conn)
            Node nodeR a -> "N " <> Id.family (Id.familyOf nodeR) <> ":" <> (UH.toString $ Id.hashOf nodeR)
            Inlet inletDef val a -> "I " <> (Id.inletRName $ _.name $ NT.unwrap inletDef)
            Outlet outletDef val a -> "O " <> (Id.outletRName $ _.name $ NT.unwrap outletDef)