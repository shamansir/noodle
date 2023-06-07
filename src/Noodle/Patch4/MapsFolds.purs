module Noodle.Patch4.MapsFolds
  where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\), type (/\))
-- import Data.Array (Array)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.FunctorWithIndex (mapWithIndex)

import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)

import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL

import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Data.UniqueHash (UniqueHash)
import Noodle.Id (Family', NodeId, familyP, InputR, class HasInputsAt, class HasOutputsAt)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Node2.MapsFolds as NMF
import Noodle.Node2.MapsFolds.Flatten (NodeLineRec, NodeLineMap)
import Noodle.Node2.MapsFolds.Repr (Repr, ToReprTop(..))
import Noodle.Patch4.MapsFolds.Repr (class FoldToReprsRec, class FoldToReprsMap, class ExtractReprs)

import Unsafe.Coerce (unsafeCoerce)


{- Helper types -}


-- newtype NodeInfo f = NodeInfo (Family' f /\ Int /\ NodeId f)

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
-}


{-
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
    HM.Mapping NoInstancesOfNodeYet node_def (Array (Node f state is os m)) where
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
    , NMF.ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodes x)) instances target
    ) <= Map (rli :: RL.RowList Type) (instances :: Row Type) (target :: Row Type) x

instance mapInstances ::
    ( RL.RowToList instances rli
    , NMF.ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapNodes x)) instances target
    ) => Map rli instances target x

class
    ( RL.RowToList instances rli
    , NMF.ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (MapNodesIndexed x) instances target
    ) <= MapI (rli :: RL.RowList Type) (instances :: Row Type) (target :: Row Type) x

instance mapInstancesIndexed ::
    ( RL.RowToList instances rli
    , NMF.ConvertNodeTo x, ConvertNodesTo (Array x)
    , HM.MapRecordWithIndex rli (MapNodesIndexed x) instances target
    ) => MapI rli instances target x


instance mappingTo ::
    ( NMF.ConvertNodeTo x ) =>
    HM.Mapping
        (MapNodes x)
        (Array (Node f state is os m))
        (Array x)
    where
    mapping MapNodes = convertNodes


instance mappingIndexedTo ::
    ( IsSymbol f, NMF.ConvertNodeIndexedTo x, ConvertNodesIndexedTo (Array x) ) =>
    HM.MappingWithIndex
        (MapNodesIndexed x)
        (Proxy f)
        (Array (Node f state is os m))
        (Array x)
    where
    mappingWithIndex MapNodesIndexed psym = convertNodesIndexed $ familyP psym

{- Fold classes and instances -}

class
    ( Monoid (ff result)
    , NMF.ConvertNodeTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (HF.ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) <= Fold (rla :: RL.RowList Type) (instances :: Row Type) (ff :: Type -> Type) result

instance foldInstances ::
    ( Monoid (ff result)
    , NMF.ConvertNodeTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (HF.ConstFolding (FoldNodes ff result)) (ff result) rla instances (ff result)
    ) => Fold rla instances ff result


class
    ( Monoid (ff result)
    , NMF.ConvertNodeIndexedTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) <= FoldI (rla :: RL.RowList Type) (instances :: Row Type) (ff :: Type -> Type) result

instance foldInstacesIndexed ::
    ( Monoid (ff result)
    , NMF.ConvertNodeIndexedTo result
    , RL.RowToList instances rla
    , HF.FoldlRecord (FoldNodesIndexed ff result) (ff result) rla instances (ff result)
    ) => FoldI rla instances ff result


instance foldNodesArr ::
    NMF.ConvertNodeTo x
    => HF.Folding
            (FoldNodes Array x)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    folding FoldNodes acc nodes = acc <> (NMF.convertNode <$> nodes)

instance foldNodesIndexedArr ::
    ( IsSymbol f, NMF.ConvertNodeIndexedTo x )
    => HF.FoldingWithIndex
            (FoldNodesIndexed Array x)
            (Proxy f)
            (Array x)
            (Array (Node f state is os m))
            (Array x)
    where
    foldingWithIndex FoldNodesIndexed psym acc nodes = acc <> Array.mapWithIndex (NMF.convertNodeIndexed $ familyP psym) nodes


instance foldNodesList ::
    NMF.ConvertNodeTo x
    => HF.Folding
            (FoldNodes List x)
            (List x)
            (Array (Node f state is os m))
            (List x)
    where
    folding FoldNodes acc nodes = acc <> (Array.toUnfoldable $ NMF.convertNode <$> nodes)


instance foldNodesIndexedList ::
    ( IsSymbol f, NMF.ConvertNodeIndexedTo x )
    => HF.FoldingWithIndex
            (FoldNodesIndexed List x)
            (Proxy f)
            (List x)
            (Array (Node f state is os m))
            (List x)
    where
    foldingWithIndex FoldNodesIndexed psym acc nodes = acc <> (mapWithIndex (NMF.convertNodeIndexed $ familyP psym) $ Array.toUnfoldable nodes)


{- Converters -}


class ConvertNodesTo x where
    convertNodes :: forall f state is os m. Array (Node f state is os m) -> x


--class ConvertNodesTo' :: Row Type -> Row Type -> Type -> Constraint
class ConvertNodesIndexedTo x where
    convertNodesIndexed :: forall f state is os m. Family' f -> Array (Node f state is os m) -> x


instance convertNodesToArray :: NMF.ConvertNodeTo x => ConvertNodesTo (Array x) where
    convertNodes arr = NMF.convertNode <$> arr

{-
-- instance convertNodesIndexedToArray :: NMF.ConvertNodeTo x => NMF.ConvertNodesIndexedTo (Array x) where
--     convertNodesIndexed fsym arr = convertNode <$> arr

instance extractShape :: (HasInputsAt is irl, HasOutputsAt os orl) => NMF.ConvertNodeTo' is os irl orl (List InputR) where
    convertNode' :: forall f state m. Node f state is os m -> List InputR
    convertNode' node = Node.inputsShape node


instance extractId :: NMF.ConvertNodeTo (NodeId f') where
    convertNode :: forall f state is os m. Node f state is os m -> NodeId f'
    convertNode node = unsafeCoerce $ Node.id node


instance extractFamily :: NMF.ConvertNodeTo (Family' f') where
    convertNode :: forall f state is os m. Node f state is os m -> Family' f'
    convertNode node = unsafeCoerce $ Node.family node


instance extractHash :: NMF.ConvertNodeTo UniqueHash where
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
-}


{- Implementations -}

init
    :: forall rln nodes instances
     . Init rln nodes instances
    => Record nodes
    -> Record instances
init = HM.hmap NoInstancesOfNodeYet


hmap
    :: forall instances rli x target
     . Map rli instances target x
    => Proxy x
    -> Record instances
    -> Record target
hmap _ = HM.hmap (MapNodes :: MapNodes x)


hmapWithIndex
    :: forall instances rli x target
     . MapI rli instances target x
    => Proxy x
    -> Record instances
    -> Record target
hmapWithIndex _ = HM.hmapWithIndex (MapNodesIndexed :: MapNodesIndexed x)


hfoldl_
    :: forall (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . Fold rla instances ff result
    => Record instances
    -> ff result
hfoldl_ = HF.hfoldl (FoldNodes :: FoldNodes ff result) (mempty :: ff result)


hfoldl
    :: forall f state is os instances rla m
     . Fold rla instances Array (Node f state is os m)
    => Record instances
    -> Array (Node f state is os m)
hfoldl = hfoldl_



hfoldlWithIndex_
    :: forall (instances :: Row Type) (rla ∷ RL.RowList Type) result (ff :: Type -> Type)
     . FoldI rla instances ff result
    => Record instances
    -> ff result
hfoldlWithIndex_ = HF.hfoldlWithIndex (FoldNodesIndexed :: FoldNodesIndexed ff result) (mempty :: ff result)


hfoldlWithIndex
    :: forall f state is os m (instances :: Row Type) (rla ∷ RL.RowList Type) (ff :: Type -> Type)
     . FoldI rla instances Array (NodeWithIndex f state is os m)
    => Record instances
    -> Array (NodeWithIndex f state is os m)
hfoldlWithIndex = hfoldlWithIndex_


toReprs
    :: forall (m :: Type -> Type) (instances :: Row Type) (rla ∷ RL.RowList Type) (reprs :: Row Type) repr
     . MonadEffect m
    => ExtractReprs m rla instances reprs repr
    => Proxy m
    -> Repr repr
    -> Record instances
    -> Record reprs
toReprs _ repr =
    HM.hmapWithIndex (ToReprTop repr :: ToReprTop m repr)


toReprsFlat
    :: forall m (instances :: Row Type) (rla ∷ RL.RowList Type) repr
     . MonadEffect m
    => FoldToReprsMap m rla instances repr
    => Proxy m
    -> Repr repr
    -> Record instances
    -> m (Array (NodeLineMap repr))
toReprsFlat _ repr =
    HF.hfoldlWithIndex (ToReprTop repr :: ToReprTop m repr) (pure [] :: m (Array (NodeLineMap repr)))