module Noodle.Patch4.MapsFolds where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)

import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL

import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Data.UniqueHash (UniqueHash)
import Noodle.Id (Family', NodeId, familyP)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node

import Unsafe.Coerce (unsafeCoerce)


{- Helper types -}


type NodesOf f state is os m = Array (Node f state is os m)

newtype NodeInfo f = NodeInfo (Family' f /\ Int /\ NodeId f)

newtype NodeWithIndex f state is os m = NodeWithIndex (Family' f /\ Int /\ Node f state is os m)


{- Inits / Maps / Folds tags -}

data NoInstancesOfNodeYet = NoInstancesOfNodeYet


data MapNodesTo :: forall k. k -> Type
data MapNodesTo x = MapNodesTo

data MapNodesIndexedTo :: forall k. k -> Type
data MapNodesIndexedTo x = MapNodesIndexedTo


data FoldNodes :: forall k. (Type -> Type) -> k -> Type
data FoldNodes (ff :: Type -> Type) x = FoldNodes

data FoldNodesIndexed :: forall k. (Type -> Type) -> k -> Type
data FoldNodesIndexed (ff :: Type -> Type) x = FoldNodesIndexed


{- Init classed and instances -}


instance initToNIONY ::
    HM.Mapping NoInstancesOfNodeYet node_def (NodesOf f state is os m) where
    mapping NoInstancesOfNodeYet = const []


class
    ( RL.RowToList nodes rln
    , HM.MapRecordWithIndex rln (HM.ConstMapping NoInstancesOfNodeYet) nodes instances
    ) <= Init rln nodes instances

instance initInstances ::
    ( RL.RowToList nodes rln
    , HM.MapRecordWithIndex rln (HM.ConstMapping NoInstancesOfNodeYet) nodes instances
    ) => Init rln nodes instances


{- Map classes and instances -}


instance mappingTo ::
  ( ConvertNodesTo x ) =>
  HM.Mapping (MapNodesTo x) (NodesOf f state is os m) x where
  mapping MapNodesTo = convertNodes


instance mappingIndexedTo ::
  ( IsSymbol f, ConvertNodesIndexedTo x ) =>
  HM.MappingWithIndex (MapNodesIndexedTo x) (Family' f) (NodesOf f state is os m) x where
  mappingWithIndex MapNodesIndexedTo = convertNodesIndexed


class Map :: RL.RowList Type -> Row Type -> Type -> Row Type -> Constraint
class
    ( RL.RowToList instances rli
    , ConvertNodesTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodesTo x)) instances result
    ) <= Map rli instances x result

instance mapInstances ::
    ( RL.RowToList instances rli
    , ConvertNodesTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodesTo x)) instances result
    ) => Map rli instances x result


class MapI :: RL.RowList Type -> Row Type -> Type -> Row Type -> Constraint
class
    ( RL.RowToList instances rli
    , ConvertNodesIndexedTo x
    , HM.MapRecordWithIndex rli (MapNodesIndexedTo x) instances result
    ) <= MapI rli instances x result

instance mapInstancesIndexed ::
    ( RL.RowToList instances rli
    , ConvertNodesIndexedTo x
    , HM.MapRecordWithIndex rli (MapNodesIndexedTo x) instances result
    ) => MapI rli instances x result


{- Fold classes and instances -}

class
    ( Monoid (ff result)
    , ConvertNodeTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (HF.ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) <= Fold rla ff result instances

instance foldInstances ::
    ( Monoid (ff result)
    , ConvertNodeTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (HF.ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) => Fold rla ff result instances


class
    ( Monoid (ff result)
    , ConvertNodeIndexed result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) <= FoldI rla ff result instances

instance foldInstacesIndexed ::
    ( Monoid (ff result)
    , ConvertNodeIndexed result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) => FoldI rla ff result instances


instance foldNodesArr ::
    ConvertNodeTo x
    => HF.Folding
            (FoldNodes Array x)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    folding FoldNodes acc nodes = acc <> (convertNode <$> nodes)

instance foldNodesIndexedArr ::
    ( IsSymbol f, ConvertNodeIndexed x )
    => HF.FoldingWithIndex
            (FoldNodesIndexed Array x)
            (Proxy f)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    foldingWithIndex FoldNodesIndexed psym acc nodes = acc <> Array.mapWithIndex (convertNodeIndexed $ familyP psym) nodes


{- Converters -}


class ConvertNodeTo x where
    convertNode :: forall f state is os m. Node f state is os m -> x


class ConvertNodeIndexed x where
    convertNodeIndexed :: forall f state is os m. IsSymbol f => Family' f -> Int -> Node f state is os m -> x


class ConvertNodesTo x where
    convertNodes :: forall f state is os m. Array (Node f state is os m) -> x


class ConvertNodesIndexedTo x where
    convertNodesIndexed :: forall f state is os m. Family' f -> Array (Node f state is os m) -> x


instance extractId :: ConvertNodeTo (NodeId f') where
    convertNode :: forall f state is os m. Node f state is os m -> NodeId f'
    convertNode node = unsafeCoerce $ Node.id node


instance extractFamily :: ConvertNodeTo (Family' f') where
    convertNode :: forall f state is os m. Node f state is os m -> Family' f'
    convertNode node = unsafeCoerce $ Node.family node


instance extractHash :: ConvertNodeTo UniqueHash where
    convertNode :: forall f state is os m. Node f state is os m -> UniqueHash
    convertNode = Node.hash


instance extractIdIndexed :: ConvertNodeIndexed (Int /\ NodeId f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> Int /\ NodeId f'
    convertNodeIndexed _ idx node = idx /\ (unsafeCoerce $ Node.id node)


instance extractIdIndexedInfo :: ConvertNodeIndexed (NodeInfo f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> NodeInfo f'
    convertNodeIndexed family idx node = NodeInfo $ unsafeCoerce family /\ idx /\ (unsafeCoerce $ Node.id node)


instance extractNodeWithIndex :: ConvertNodeIndexed (NodeWithIndex f' state' is' os' m') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> NodeWithIndex f' state' is' os' m'
    convertNodeIndexed family idx node = NodeWithIndex $ unsafeCoerce family /\ idx /\ unsafeCoerce node


instance convertToItself :: ConvertNodeTo (Node f' state' is' os' m') where
    convertNode :: forall f state is os m. Node f state is os m -> Node f' state' is' os' m'
    -- convertNode :: Node f' state' is' os' m' -> Node f' state' is' os' m'
    convertNode = unsafeCoerce
    -- convertNode = identity


instance convertIndexedToItself :: ConvertNodeIndexed (Node f' state' is' os' m') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> Node f' state' is' os' m'
    convertNodeIndexed _ _ = unsafeCoerce



{- Implementations -}

init
    :: forall rln nodes instances
     . Init rln nodes instances
    => Record nodes
    -> Record instances
init = HM.hmap NoInstancesOfNodeYet


hmap
    :: forall instances rli x result
     . Map rli instances x result
    => Record instances
    -> Record result
hmap = HM.hmap (MapNodesTo :: MapNodesTo x)


hmapWithIndex
    :: forall instances rli x result
     . MapI rli instances x result
    => Record instances
    -> Record result
hmapWithIndex = HM.hmapWithIndex (MapNodesIndexedTo :: MapNodesIndexedTo x)


hfoldl_
    :: forall (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . Fold rla ff result instances
    => Record instances
    -> ff result
hfoldl_ = HF.hfoldl (FoldNodes :: FoldNodes ff result) (mempty :: ff result)


hfoldl
    :: forall f state is os instances rla m
     . Fold rla Array (Node f state is os m) instances
    => Record instances
    -> Array (Node f state is os m)
hfoldl = hfoldl_



hfoldlWithIndex_
    :: forall (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . FoldI rla ff result instances
    => Record instances
    -> ff result
hfoldlWithIndex_ = HF.hfoldlWithIndex (FoldNodesIndexed :: FoldNodesIndexed ff result) (mempty :: ff result)


hfoldlWithIndex
    :: forall f state is os m (instances :: Row Type) (rla ∷ RL.RowList Type) (ff :: Type -> Type)
     . FoldI rla Array (NodeWithIndex f state is os m) instances
    => Record instances
    -> Array (NodeWithIndex f state is os m)
hfoldlWithIndex = hfoldlWithIndex_