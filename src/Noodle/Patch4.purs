module Noodle.Patch4 where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Const (Const)
import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\) )
import Data.Unfoldable (class Unfoldable)
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy (Proxy)

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



data FoldNodes (m :: Type -> Type) x = FoldNodes

data FoldNodesIndexed (m :: Type -> Type) x = FoldNodesIndexed


instance foldNodes ::
    ( Unfoldable u, Semigroup (u x), ConvertNodeTo x )
    => Folding
            (FoldNodes u x)
            (u x)
            (Array (Node f state is os m))
            (u x)
    where
    folding FoldNodes acc nodes = acc <> (Array.toUnfoldable $ convertNode <$> nodes)


instance foldNodesIndexed ::
    ( Unfoldable u, Semigroup (u x), IsSymbol sym, ConvertNodeIndexed x )
    => FoldingWithIndex
            (FoldNodesIndexed u x)
            (Proxy sym)
            (u x)
            (Array (Node f state is os m))
            (u x)
    where
    foldingWithIndex FoldNodesIndexed i acc nodes = acc <> (Array.toUnfoldable $ Array.mapWithIndex (convertNodeIndexed i) nodes)


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


{-}
nodes_
    :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) result (folding :: Type -> Type)
     . RL.RowToList instances rla
    => ConvertNodeTo result
    => FoldlRecord (ConstFolding (folding result)) (Array result) rla instances (Array result)
    => folding result
    -> Patch ps instances
    -> Array result
nodes_ a (Patch _ instances _) =
    hfoldl a ([] :: Array result) instances -}


nodes_
    :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) result (folding :: (Type -> Type) -> Type -> Type) (m :: Type -> Type)
     . RL.RowToList instances rla
    => Monoid (m result)
    => ConvertNodeTo result
    => FoldlRecord (ConstFolding (folding m result)) (m result) rla instances (m result)
    => folding m result
    -> Patch ps instances
    -> m result
nodes_ a (Patch _ instances _) =
    hfoldl a (mempty :: m result) instances


nodesIndexed
    :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) result (folding :: (Type -> Type) -> Type -> Type) (m :: Type -> Type)
     . RL.RowToList instances rla
    => Monoid (m result)
    => ConvertNodeIndexed result
    => FoldlRecord (folding m result) (m result) rla instances (m result)
    => folding m result
    -> Patch ps instances
    -> m result
nodesIndexed a (Patch _ instances _) =
    hfoldlWithIndex a (mempty :: m result) instances



-- testNodes ∷ ∀ (t234 ∷ Type) (t235 ∷ Row Type) (t236 ∷ RowList Type). RowToList @Type t235 t236 ⇒ FoldlRecord (ConstFolding (FoldNodes @Type String)) String t236 t235 String ⇒ Patch t234 t235 → String
testNodes patch = String.joinWith "--" $ testNodes' patch


testNodes' patch = (nodes_ FoldNodes patch :: Array String)


testNodesIndexed
    :: forall state instances rl
     . RL.RowToList instances rl
    => ConvertNodeIndexed String
    => FoldlRecord (FoldNodesIndexed Array String) (Array String) rl instances (Array String)
    => Patch state instances
    -> Array String
testNodesIndexed patch = (nodesIndexed (FoldNodesIndexed :: FoldNodesIndexed Array String) patch :: Array String)



-- testNodes''' patch = (nodesIndexed FoldNodesIndexed patch :: Array String)



-- nodes
--     :: forall ps (instances :: Row Type) (rla ∷ RL.RowList Type) t x
--      . RL.RowToList instances rla
--     -- => HFoldl FoldNodes (Array _) (Record instances) (Array x)
--     => FoldlRecord FoldNodes _ rla instances (Array x)
--     => (forall f state is os m. Node.Family f -> NodesOf f state is os m -> x)
--     -> Patch ps instances
--     -> Array x
-- nodes fn (Patch _ instances _) =
--     hfoldlWithIndex FoldNodes fn instances


-- families ::


class ConvertNodeTo x where
    convertNode :: forall f state is os m. Node f state is os m -> x


class ConvertNodeIndexed x where
    convertNodeIndexed :: forall sym f state is os m. IsSymbol sym => Proxy sym -> Int -> Node f state is os m -> x


instance extractId :: ConvertNodeTo String where
    convertNode = Node.hash


instance extractIdIndexed :: ConvertNodeIndexed String where
    convertNodeIndexed sym idx node = reflectSymbol sym <> "::" <> show idx <> "::" <> Node.hash node


-- nodes :: forall t218 t219 t220 t224 t227 x. RL.RowToList t220 t227 => FoldlRecord (ConstFolding FoldNodes) x t227 t220 x => ConverNodeTo x => Patch t219 t220 -> x
-- nodes (Patch _ instances _) =
--     hfoldl ?wh empty ?wh




-- nodes :: forall ps instances rli. Record.Keys rli => RL.RowToList instances rli => Patch ps instances -> Array (forall f state is os m. String /\ NodesOf f state is os m)
-- nodes (Patch _ instances _) =
--     Array.fromFoldable ((unsafeGet >>> unsafeCoerce) <$> Record.keys instances)
--     where
--         unsafeGet key = key /\ Record.unsafeGet key instances