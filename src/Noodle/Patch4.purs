module Noodle.Patch4 where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Const (Const)
import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\) )
import Unsafe.Coerce (unsafeCoerce)

import Record.Extra as Record
import Record.Unsafe as Record

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

import Heterogeneous.Folding
    ( class HFoldl
    , class HFoldlWithIndex
    , class Folding
    , class FoldingWithIndex
    , class FoldlRecord
    , ConstFolding
    , hfoldl, hfoldlWithIndex
    -- , class HFoldlWithIndex
    )

import Record as Record
import Record.Builder (Builder)

import Unsafe.Coerce (unsafeCoerce)

import Data.Exists (Exists, mkExists, runExists)


type NodesOf f state is os m = Array (Node f state is os m)


--data LinkOE fo fi = Exists (LinkOf fo fi)

type Id = String


data NoInstancesOfNodeYet = NoInstancesOfNodeYet


type Links =
  { from :: Map Node.FromId (forall fo fi i o. Node.Link fo fi i o)
  , to :: Map Node.ToId (forall fo fi i o. Node.Link fo fi i o)
  }


data Patch state (instances :: Row Type) = Patch state (Record instances) Links


instance mappingToNIONY ::
  Mapping NoInstancesOfNodeYet node_def (NodesOf f state is os m) where
  mapping NoInstancesOfNodeYet = const []


data ExtractInstances = ExtractInstances



data FoldInstancesToPlane x = FoldInstancesToPlane


-- instance foldNodes ::
--   FoldingWithIndex FoldInstancesToPlane i (Array (Node f state is os m)) (NodesOf f state is os m) (Array (Node f state is os m)) where
--   foldingWithIndex FoldInstancesToPlane _ acc nodes = acc <> nodes



-- instance foldNodes ::
--   Folding (FoldInstancesToPlane String) (Array (Node f state is os m)) (NodesOf f state is os m) (Array (Node f state is os m)) where
--   folding FoldInstancesToPlane acc nodes = acc <> nodes


instance foldNodes ::
  ConvertNodeTo String => Folding (FoldInstancesToPlane String) String (Array (Node f state is os m)) String where
    -- folding FoldInstancesToPlane acc nodes = acc <> "foo"
    folding FoldInstancesToPlane acc nodes = acc <> "jjj" <> (String.joinWith "-" (map (const "afoo") nodes))


-- instance foldNodesI ::
--   FoldingWithIndex (FoldInstancesToPlane String) i (Array (Node f state is os m)) (NodesOf f state is os m) (Array (Node f state is os m)) where
--   foldingWithIndex FoldInstancesToPlane _ acc nodes = acc <> nodes


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
    => Toolkit Unit nodes
    -> Patch Unit instances
init = init' unit


init'
    :: forall
        state
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . RL.RowToList nodes rln
    => MapRecordWithIndex rln (ConstMapping NoInstancesOfNodeYet) nodes instances
    => state
    -> Toolkit state nodes
    -> Patch state instances
init' state tk =
    Patch
        state
        (hmap NoInstancesOfNodeYet $ Toolkit.toRecord tk)
        { from : Map.empty
        , to : Map.empty
        }


registerNode
    :: forall ps instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
registerNode node (Patch state instances links) =
    Patch
        state
        (Record.modify (Node.family node) ((:) node) instances)
        links


nodesOf
    :: forall ps instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch ps instances
    -> NodesOf f state is os m
nodesOf family (Patch _ instances _) = Record.get family instances


howMany
    :: forall ps instances' instances f state is os m
     . Row.Cons f (NodesOf f state is os m) instances' instances
    => IsSymbol f
    => Node.Family f
    -> Patch ps instances
    -> Int
howMany f = nodesOf f >>> Array.length


registerLink
    :: forall ps instances fo fi i o
     . IsSymbol fo
    => IsSymbol fi
    => IsSymbol i
    => IsSymbol o
    => Node.Link fo fi i o
    -> Patch ps instances
    -> Patch ps instances
registerLink link (Patch state instances links) =
  Patch
    state
    instances
    { from : Map.insert (Node.toFromId link) (unsafeCoerce link) links.from
    , to : Map.insert (Node.toToId link) (unsafeCoerce link) links.to
    }


nodes_
    :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) result (folding :: Type -> Type)
     . RL.RowToList instances rla
    => ConvertNodeTo result
    => FoldlRecord (ConstFolding (folding result)) result rla instances result
    => folding result
    -> Patch ps instances
    -> result
nodes_ a (Patch _ instances _) =
    hfoldl a (empty_ :: result) instances



--testNodes ∷ ∀ (t234 ∷ Type) (t235 ∷ Row Type) (t236 ∷ RowList Type). RowToList @Type t235 t236 ⇒ FoldlRecord (ConstFolding (FoldInstancesToPlane @Type String)) String t236 t235 String ⇒ Patch t234 t235 → String
testNodes patch = (nodes_ FoldInstancesToPlane patch :: String)



-- nodes
--     :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) t x
--      . RL.RowToList instances rla
--     -- => HFoldl FoldInstancesToPlane (Array _) (Record instances) (Array x)
--     => FoldlRecord FoldInstancesToPlane _ rla instances (Array x)
--     => (forall f state is os m. Node.Family f -> NodesOf f state is os m -> x)
--     -> Patch ps instances
--     -> Array x
-- nodes fn (Patch _ instances _) =
--     hfoldlWithIndex FoldInstancesToPlane fn instances


-- families ::


class ConvertNodeTo x where
    empty_ :: x
    convertNode :: forall f state is os m. Node f state is os m -> x


instance extractId :: ConvertNodeTo String where
    empty_ = "aaa"
    convertNode = Node.hash


-- nodes :: forall t218 t219 t220 t224 t227 x. RL.RowToList t220 t227 => FoldlRecord (ConstFolding FoldInstancesToPlane) x t227 t220 x => ConverNodeTo x => Patch t219 t220 -> x
-- nodes (Patch _ instances _) =
--     hfoldl ?wh empty ?wh




-- nodes :: forall ps instances rli. Record.Keys rli => RL.RowToList instances rli => Patch ps instances -> Array (forall f state is os m. String /\ NodesOf f state is os m)
-- nodes (Patch _ instances _) =
--     Array.fromFoldable ((unsafeGet >>> unsafeCoerce) <$> Record.keys instances)
--     where
--         unsafeGet key = key /\ Record.unsafeGet key instances