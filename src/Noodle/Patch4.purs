module Noodle.Patch4 where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Const (Const)
import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map

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

import Data.Exists (Exists, mkExists, runExists)


type NodesOf f state is os m = Array (Node f state is os m)


--data LinkOE fo fi = Exists (LinkOf fo fi)


data NoInstancesOfNodeYet = NoInstancesOfNodeYet


type Links =
  { from :: Map Node.FromId (forall fo fi i o. Node.Link fo fi i o)
  , to :: Map Node.ToId (forall fo fi i o. Node.Link fo fi i o)
  }


data Patch (instances :: Row Type) = Patch (Record instances) Links


instance mappingToNIONY ::
  Mapping NoInstancesOfNodeYet a (NodesOf f state is os m) where
  mapping NoInstancesOfNodeYet = const []


{- instance mappingIndexedToNIONY ::
  MappingWithIndex NoInstancesOfNodeYet k a (Array (Node state is os m)) where
  mappingWithIndex NoInstancesOfNodeYet = const $ const [] -}


init
    :: forall
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . RL.RowToList nodes rln
    => MapRecordWithIndex rln (ConstMapping NoInstancesOfNodeYet) nodes instances
    => Toolkit nodes
    -> Patch instances
init tk =
    Patch
        (hmap NoInstancesOfNodeYet $ Toolkit.toRecord tk)
        { from : Map.empty
        , to : Map.empty
        }


registerNode
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node f state is os m
    -> Patch instances
    -> Patch instances
registerNode node (Patch instances links) =
    Patch
        (Record.modify (Node.family node) ((:) node) instances)
        links


nodesOf
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances
    -> NodesOf f state is os m
nodesOf family (Patch instances _) = Record.get family instances


howMany
    :: forall instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances
    -> Int
howMany f = nodesOf f >>> Array.length


registerLink
    :: forall instances fo fi i o
     . IsSymbol fo
    => IsSymbol fi
    => IsSymbol i
    => IsSymbol o
    => Node.Link fo fi i o
    -> Patch instances
    -> Patch instances
registerLink link (Patch instances links) =
  Patch
    instances
    { from : Map.insert (Node.toFromId link) (unsafeCoerce link) links.from
    , to : Map.insert (Node.toToId link) (unsafeCoerce link) links.to
    }