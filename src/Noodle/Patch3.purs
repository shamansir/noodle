module Noodle.Patch3 where

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

import Data.Exists (Exists, mkExists, runExists)


type NodesOf f state is os m = Array (Node f state is os m)


-- type LinkE fo fi = Exists (Node.Link fo fi)

-- data LinkOf fo fi (i :: Symbol) (o :: Symbol) = LinkOf (Node.Input i -> Node.Output o -> Node.Link fo fi i o)
data LinksFrom fo o = LinksFrom (Node.NodeId fo -> Node.Output o -> Array (forall fi i. Node.Link fo fi i o))

data LinksTo fi i = LinksTo (Node.NodeId fi -> Node.Input i -> Array (forall fo o. Node.Link fo fi i o))


--data LinkOE fo fi = Exists (LinkOf fo fi)


data NoInstancesOfNodeYet = NoInstancesOfNodeYet
data NoLinksFromYet = NoLinksFromYet
data NoLinksToYet = NoLinksToYet


data Patch (instances :: Row Type) (links_from :: Row Type) (links_to :: Row Type) = Patch (Record instances) (Record links_from) (Record links_to)


instance mappingToNIONY ::
  Mapping NoInstancesOfNodeYet a (NodesOf f state is os m) where
  mapping NoInstancesOfNodeYet = const []

instance mappingToLinksFrom ::
  Mapping NoLinksFromYet a (LinksFrom fo o) where
  mapping NoLinksFromYet = const $ LinksFrom \_ _ -> [] :: forall fi i. Array (Node.Link fo fi i o)

instance mappingToLinksTo ::
  Mapping NoLinksToYet a (LinksTo fi i) where
  mapping NoLinksToYet = const $ LinksTo \_ _ -> [] :: forall fo o. Array (Node.Link fo fi i o)


{- instance mappingIndexedToNIONY ::
  MappingWithIndex NoInstancesOfNodeYet k a (Array (Node state is os m)) where
  mappingWithIndex NoInstancesOfNodeYet = const $ const [] -}


init
    :: forall
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (links_from :: Row Type)
        (links_to :: Row Type)
        (rln ∷ RL.RowList Type)
        (rllf ∷ RL.RowList Type)
        (rllt ∷ RL.RowList Type)
     . RL.RowToList nodes rln
    => RL.RowToList links_from rllf
    => RL.RowToList links_to rllt
    => MapRecordWithIndex rln (ConstMapping NoInstancesOfNodeYet) nodes instances
    => MapRecordWithIndex rln (ConstMapping NoLinksFromYet) nodes links_from
    => MapRecordWithIndex rln (ConstMapping NoLinksToYet) nodes links_to
    => Toolkit nodes
    -> Patch instances links_from links_to
init tk =
    Patch
        (hmap NoInstancesOfNodeYet $ Toolkit.toRecord tk)
        (hmap NoLinksFromYet $ Toolkit.toRecord tk)
        (hmap NoLinksToYet $ Toolkit.toRecord tk)


registerNode
    :: forall instances' instances lt lf f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node f state is os m
    -> Patch instances lt lf
    -> Patch instances lt lf
registerNode node (Patch instances lf lt) =
    Patch
        (Record.modify (Node.family node) ((:) node) instances)
        lf
        lt


nodesOf
    :: forall instances' instances lt lf f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances lt lf
    -> NodesOf f state is os m
nodesOf family (Patch instances _ _) = Record.get family instances


howMany
    :: forall instances' instances lt lf f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch instances lt lf
    -> Int
howMany f = nodesOf f >>> Array.length