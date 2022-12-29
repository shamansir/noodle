module Noodle.Patch2 where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Const (Const)
import Data.Array ((:))
import Data.Array as Array

import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit


import Prim.Row (class Cons) as Row
import Prim.RowList as RL

import Heterogeneous.Mapping
    ( class HMapWithIndex
    , class Mapping
    , class MappingWithIndex
    , class MapRecordWithIndex
    , ConstMapping
    , hmap, hmapWithIndex
    -- , class HFoldlWithIndex
    )

import Record as Record
import Record.Builder (Builder)

import Unsafe.Coerce (unsafeCoerce)


type NodesOf f state is os m = Array (Node f state is os m)


data NoInstancesOfNodeYet = NoInstancesOfNodeYet


data Patch (instances :: Row Type)  = Patch (Record instances)


instance mappingToNIONY ::
  Mapping NoInstancesOfNodeYet a (NodesOf f state is os m) where
  mapping NoInstancesOfNodeYet = const []


{- instance mappingIndexedToNIONY ::
  MappingWithIndex NoInstancesOfNodeYet k a (Array (Node state is os m)) where
  mappingWithIndex NoInstancesOfNodeYet = const $ const [] -}


init
    :: forall (instances ∷ Row Type) (nodes ∷ Row Type) (rl ∷ RL.RowList Type)
     . RL.RowToList nodes rl
    => MapRecordWithIndex rl (ConstMapping NoInstancesOfNodeYet) nodes instances
    => Toolkit nodes
    -> Patch instances
init tk = Patch $ hmap NoInstancesOfNodeYet $ Toolkit.toRecord tk


registerNode
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node f state is os m
    -> Patch instances
    -> Patch instances
registerNode node (Patch instances) = Patch $ Record.modify (Node.family node) ((:) node) instances


nodesOf
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances
    -> NodesOf f state is os m
nodesOf family (Patch instances) = Record.get family instances


howMany
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances
    -> Int
howMany f = nodesOf f >>> Array.length