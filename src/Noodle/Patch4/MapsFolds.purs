module Noodle.Patch4.MapsFolds
  where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
-- import Data.Array (Array)
import Data.Array as Array
import Data.List (List)
import Data.List as List

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




{- data MapTarget
foreign import data To :: Type -> MapTarget -}

{-
data FoldTarget
foreign import data FoldTo :: (Type -> Type) -> Type -> FoldTarget
-}


{- data Target
foreign import data ForMap :: MapTarget -> Target
foreign import data ForFold :: FoldTarget -> Target -}

{-
data Focus
foreign import data Empty :: Focus
foreign import data OnInlets :: Row Type -> Focus
foreign import data OnOutlets :: Row Type -> Focus
foreign import data OnInletsOutlets :: Row Type -> Row Type -> Focus


data MapNodes :: Focus -> Type -> Type
data MapNodes (focus :: Focus) x = MapNodes

data MapNodesIndexed :: Focus -> Type -> Type
data MapNodesIndexed (focus :: Focus) x = MapNodesIndexed


data FoldNodes :: Focus -> FoldTarget -> Type
data FoldNodes (focus :: Focus) (x :: FoldTarget) = FoldNodes

data FoldNodesIndexed :: Focus -> FoldTarget -> Type
data FoldNodesIndexed (focus :: Focus) (x :: FoldTarget) = FoldNodesIndexed
-}

data MapNodes x = MapNodes

data MapNodesIndexed x = MapNodesIndexed


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


class
    ( RL.RowToList instances rli
    , ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodes x)) instances rrow
    ) <= Map rli instances x rrow

instance mapInstances ::
    ( RL.RowToList instances rli
    , ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodes x)) instances rrow
    ) => Map rli instances x rrow


class
    ( RL.RowToList instances rli
    , ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (MapNodesIndexed x) instances rrow
    ) <= MapI rli instances x rrow

instance mapInstancesIndexed ::
    ( RL.RowToList instances rli
    , ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (MapNodesIndexed x) instances rrow
    ) => MapI rli instances x rrow


instance mappingTo ::
    ( ConvertNodeTo x ) =>
    HM.Mapping
        (MapNodes x)
        (NodesOf f state is os m)
        (Array x)
    where
    mapping MapNodes = convertNodes


instance mappingIndexedTo ::
    ( IsSymbol f, ConvertNodeIndexedTo x, ConvertNodesIndexedTo (Array x) ) =>
    HM.MappingWithIndex
        (MapNodesIndexed x)
        (Proxy f)
        (NodesOf f state is os m)
        (Array x)
    where
    mappingWithIndex MapNodesIndexed psym = convertNodesIndexed $ familyP psym

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
    , ConvertNodeIndexedTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) <= FoldI rla ff result instances

instance foldInstacesIndexed ::
    ( Monoid (ff result)
    , ConvertNodeIndexedTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) => FoldI rla ff result instances


instance foldNodesArr ::
    ConvertNodeTo x
    => HF.Folding
            (FoldNodes Array x)
            (Array x)
            (NodesOf f state is os m)
            (Array x)
    where
    folding FoldNodes acc nodes = acc <> (convertNode <$> nodes)

instance foldNodesIndexedArr ::
    ( IsSymbol f, ConvertNodeIndexedTo x )
    => HF.FoldingWithIndex
            (FoldNodesIndexed Array x)
            (Proxy f)
            (Array x)
            (NodesOf f state is os m)
            (Array x)
    where
    foldingWithIndex FoldNodesIndexed psym acc nodes = acc <> Array.mapWithIndex (convertNodeIndexed $ familyP psym) nodes


instance foldNodesList ::
    ConvertNodeTo x
    => HF.Folding
            (FoldNodes List x)
            (List x)
            (NodesOf f state is os m)
            (List x)
    where
    folding FoldNodes acc nodes = acc <> (Array.toUnfoldable $ convertNode <$> nodes)


instance foldNodesIndexedList ::
    ( IsSymbol f, ConvertNodeIndexedTo x )
    => HF.FoldingWithIndex
            (FoldNodesIndexed List x)
            (Proxy f)
            (List x)
            (NodesOf f state is os m)
            (List x)
    where
    foldingWithIndex FoldNodesIndexed psym acc nodes = acc <> (List.mapWithIndex (convertNodeIndexed $ familyP psym) $ Array.toUnfoldable nodes)


{- Converters -}


-- class ConvertNodeTo (focus :: Focus) x where
class ConvertNodeTo x where
    convertNode :: forall f state is os m. Node f state is os m -> x


class ConvertNodeIndexedTo x where
    convertNodeIndexed :: forall f state is os m. IsSymbol f => Family' f -> Int -> Node f state is os m -> x


class ConvertNodesTo x where
    convertNodes :: forall f state is os m. Array (Node f state is os m) -> x


class ConvertNodesIndexedTo x where
    convertNodesIndexed :: forall f state is os m. Family' f -> Array (Node f state is os m) -> x


instance convertNodesToArray :: ConvertNodeTo x => ConvertNodesTo (Array x) where
    convertNodes arr = convertNode <$> arr


-- instance convertNodesIndexedToArray :: ConvertNodeTo x => ConvertNodesIndexedTo (Array x) where
--     convertNodesIndexed fsym arr = convertNode <$> arr


instance extractId :: ConvertNodeTo (NodeId f') where
    convertNode :: forall f state is os m. Node f state is os m -> NodeId f'
    convertNode node = unsafeCoerce $ Node.id node


instance extractFamily :: ConvertNodeTo (Family' f') where
    convertNode :: forall f state is os m. Node f state is os m -> Family' f'
    convertNode node = unsafeCoerce $ Node.family node


instance extractHash :: ConvertNodeTo UniqueHash where
    convertNode :: forall f state is os m. Node f state is os m -> UniqueHash
    convertNode = Node.hash


instance extractIdIndexed :: ConvertNodeIndexedTo (Int /\ NodeId f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> Int /\ NodeId f'
    convertNodeIndexed _ idx node = idx /\ (unsafeCoerce $ Node.id node)


instance extractIdIndexedInfo :: ConvertNodeIndexedTo (NodeInfo f') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> NodeInfo f'
    convertNodeIndexed family idx node = NodeInfo $ unsafeCoerce family /\ idx /\ (unsafeCoerce $ Node.id node)


instance extractNodeWithIndex :: ConvertNodeIndexedTo (NodeWithIndex f' state' is' os' m') where
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


instance convertIndexedToItself :: ConvertNodeIndexedTo (Node f' state' is' os' m') where
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
    :: forall instances rli x rrow
     . Map rli instances x rrow
    => Proxy x
    -> Record instances
    -> Record rrow
hmap _ = HM.hmap (MapNodes :: MapNodes x)


hmapWithIndex
    :: forall instances rli x rrow
     . MapI rli instances x rrow
    => Proxy x
    -> Record instances
    -> Record rrow
hmapWithIndex _ = HM.hmapWithIndex (MapNodesIndexed :: MapNodesIndexed x)


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