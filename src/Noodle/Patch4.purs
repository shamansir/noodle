module Noodle.Patch4 where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
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
import Data.Identity (Identity)
import Data.UniqueHash (UniqueHash)

import Record.Extra as Record
import Record.Unsafe as Record

import Noodle.Id
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


data Patch gstate (instances :: Row Type) = Patch gstate (Record instances) Links


instance mappingToNIONY ::
  Mapping NoInstancesOfNodeYet node_def (NodesOf f state is os m) where
  mapping NoInstancesOfNodeYet = const []


data FoldNodes :: forall k. (Type -> Type) -> k -> Type
data FoldNodes (ff :: Type -> Type) x = FoldNodes


data FoldNodesIndexed :: forall k. (Type -> Type) -> k -> Type
data FoldNodesIndexed (ff :: Type -> Type) x = FoldNodesIndexed


class
    ( RL.RowToList nodes rln
    , MapRecordWithIndex rln (ConstMapping NoInstancesOfNodeYet) nodes instances
    ) <= Map rln nodes instances

instance map ::
    ( RL.RowToList nodes rln
    , MapRecordWithIndex rln (ConstMapping NoInstancesOfNodeYet) nodes instances
    ) => Map rln nodes instances

class
    ( Monoid (ff result)
    , ConvertNodeTo result
    , RL.RowToList instances rla
    , FoldlRecord (ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) <= Fold rla ff result instances

instance fold ::
    ( Monoid (ff result)
    , ConvertNodeTo result
    , RL.RowToList instances rla
    , FoldlRecord (ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) => Fold rla ff result instances

class
    ( Monoid (ff result)
    , ConvertNodeIndexed result
    , RL.RowToList instances rla
    , FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) <= FoldI rla ff result instances

instance foldI ::
    ( Monoid (ff result)
    , ConvertNodeIndexed result
    , RL.RowToList instances rla
    , FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) => FoldI rla ff result instances


{-
instance foldNodes ::
    ( Unfoldable u, Semigroup (u x), ConvertNodeTo x )
    => Folding
            (FoldNodes u x)
            (u x)
            (Array (Node f state is os m))
            (u x)
    where
    folding FoldNodes acc nodes = acc <> (Array.toUnfoldable $ convertNode <$> nodes)
-}

instance foldNodesArr ::
    ConvertNodeTo x
    => Folding
            (FoldNodes Array x)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    folding FoldNodes acc nodes = acc <> (convertNode <$> nodes)


{-
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
-}


instance foldNodesIndexedArr ::
    ( IsSymbol f, ConvertNodeIndexed x )
    => FoldingWithIndex
            (FoldNodesIndexed Array x)
            (Proxy f)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    foldingWithIndex FoldNodesIndexed i acc nodes = acc <> Array.mapWithIndex (convertNodeIndexed i) nodes


class
    ( IsSymbol f
    , Row.Cons f x instances' instances
    )
    <= HasInstancesOf f instances' instances x -- FIXME: use newtype
instance
    ( IsSymbol f
    , Row.Cons f x instances' instances
    )
    => HasInstancesOf f instances' instances x -- FIXME: use newtype


init
    :: forall
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . Map rln nodes instances
    => Toolkit Unit nodes
    -> Patch Unit instances
init = init' unit


init'
    :: forall
        gstate
        (instances ∷ Row Type)
        (nodes ∷ Row Type)
        (rln ∷ RL.RowList Type)
     . Map rln nodes instances
    => gstate
    -> Toolkit gstate nodes
    -> Patch gstate instances
init' state tk =
    Patch
        state
        (hmap NoInstancesOfNodeYet $ Toolkit.toRecord tk)
        { from : Map.empty
        , to : Map.empty
        }


registerNode
    :: forall ps instances' instances f state is os m
     . HasInstancesOf f instances' instances (NodesOf f state is os m)
    => Node f state is os m
    -> Patch ps instances
    -> Patch ps instances
registerNode node (Patch state instances links) =
    Patch
        state
        (Record.modify (Node.family node) ((:) node) instances) -- NB: notice that Family' f works!
        links


nodesOf
    :: forall ps instances' instances f state is os m
     . HasInstancesOf f instances' instances (NodesOf f state is os m)
    => Family f
    -> Patch ps instances
    -> NodesOf f state is os m
nodesOf family (Patch _ instances _) = Record.get family instances


howMany
    :: forall ps instances' instances f state is os m
     . HasInstancesOf f instances' instances (NodesOf f state is os m)
    => Family f
    -> Patch ps instances
    -> Int
howMany f = nodesOf f >>> Array.length


registerLink
    :: forall gstate instances fo fi i o
     . Node.Link fo fi i o
    -> Patch gstate instances
    -> Patch gstate instances
registerLink link (Patch state instances links) =
  Patch
    state
    instances
    { from : Map.insert (Node.toFromId link) (unsafeCoerce link) links.from
    , to : Map.insert (Node.toToId link) (unsafeCoerce link) links.to
    }


nodes_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (m :: Type -> Type)
     . Fold rla m result instances
    => Patch gstate instances
    -> m result
nodes_ (Patch _ instances _) =
    hfoldl (FoldNodes :: FoldNodes m result) (mempty :: m result) instances


nodes
    :: forall f state is os gstate instances rla m
     . Fold rla Array (Node f state is os m) instances
    => Patch gstate instances
    -> Array (Node f state is os m)
nodes patch =
    (nodes_ patch :: Array (Node _ _ _ _ m))


nodesIndexed_
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (folding :: (Type -> Type) -> Type -> Type) (m :: Type -> Type)
     . RL.RowToList instances rla
    => Monoid (m result)
    => ConvertNodeIndexed result
    => FoldlRecord (folding m result) (m result) rla instances (m result)
    => folding m result
    -> Patch gstate instances
    -> m result
nodesIndexed_ a (Patch _ instances _) =
    hfoldlWithIndex a (mempty :: m result) instances


nodesIndexed
    :: forall gstate (instances :: Row Type) (rla ∷ RL.RowList Type) result (m :: Type -> Type)
     . FoldI rla m result instances
    => Patch gstate instances
    -> m result
nodesIndexed = nodesIndexed_ FoldNodesIndexed


-- families ::


class ConvertNodeTo x where
    convertNode :: forall f state is os m. Node f state is os m -> x


class ConvertNodeIndexed x where
    convertNodeIndexed :: forall f state is os m. IsSymbol f => Proxy f -> Int -> Node f state is os m -> x



instance extractId :: ConvertNodeTo (NodeId f') where
    convertNode :: forall f state is os m. Node f state is os m -> NodeId f'
    convertNode node = unsafeCoerce $ Node.id node


instance extractFamily :: ConvertNodeTo (Family' f') where
    convertNode :: forall f state is os m. Node f state is os m -> Family' f'
    convertNode node = unsafeCoerce $ Node.family node


instance extractHash :: ConvertNodeTo UniqueHash where
    convertNode :: forall f state is os m. Node f state is os m -> UniqueHash
    convertNode = Node.hash


newtype NodeInfo f = NodeInfo (Family' f /\ Int /\ NodeId f)


instance extractIdIndexed :: ConvertNodeIndexed (Int /\ NodeId f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Proxy f
        -> Int
        -> Node f state is os m
        -> Int /\ NodeId f'
    convertNodeIndexed _ idx node = idx /\ (unsafeCoerce $ Node.id node)


instance extractIdIndexed' :: ConvertNodeIndexed (NodeInfo f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Proxy f
        -> Int
        -> Node f state is os m
        -> NodeInfo f'
    convertNodeIndexed psym idx node = NodeInfo $ (unsafeCoerce $ familyP psym) /\ idx /\ (unsafeCoerce $ Node.id node)


instance convertToItself :: ConvertNodeTo (Node f state is os m) where
    convertNode = unsafeCoerce


instance convertIndexedToItself :: ConvertNodeIndexed (Node f state is os m) where
    convertNodeIndexed _ _ = unsafeCoerce