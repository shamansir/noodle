module Noodle.Patch4.MapsFolds.Repr
  ( Repr(..)
  , ToReprDownI
  , ToReprTop(..)
  , class HasRepr
  , toRepr
  , NodeLineRec
  , NodeLineMap
  , class FoldToReprsRec
  , class FoldToReprsMap
  )
  where

import Prelude

import Effect.Class (class MonadEffect)
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
--import Data.FoldableWithIndex (foldlWithIndex)
import Data.TraversableWithIndex (traverseWithIndex)
import Prim.RowList as RL


import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Noodle.Id (NodeId, familyP, inputP, outputP, InputR, OutputR, Input, inputR', outputR')
import Noodle.Family.Def as Family
import Noodle.Patch4.Path (Path(..))
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn
import Noodle.Node2 (Node)
import Noodle.Node2 as Node



data ToReprTop :: forall k. k -> Type
data ToReprTop repr = ToReprTop (Repr repr)
data ToReprDownI :: forall k. Symbol -> k -> Type
data ToReprDownI f repr = ToReprDownI (NodeId f) (Repr repr)
data ToReprDownO :: forall k. Symbol -> k -> Type
data ToReprDownO f repr = ToReprDownO (NodeId f) (Repr repr)


data Repr :: forall k. k -> Type
data Repr a = Repr



class HasRepr a repr where
    toRepr :: forall f i o. Path f i o -> a -> repr -- include Repr as kind here?


type NodeLineRec f repr repr_is repr_os =
    NodeId f /\ repr /\ Record repr_is /\ Record repr_os

type NodeLineMap f repr =
    NodeId f /\ repr /\ Map InputR repr /\ Map OutputR repr


instance foldToReprsRec ::
    ( Semigroup (m (Array (NodeLineRec f repr repr_is repr_os)))
    , MonadEffect m
    , ToReprFoldToRecsHelper f is iks os oks repr_is repr_os repr state
    )
    => HF.FoldingWithIndex
            (ToReprTop repr)
            (Proxy sym)
            (m (Array (NodeLineRec f repr repr_is repr_os)))
            (Array (Node f state is os m))
            (m (Array (NodeLineRec f repr repr_is repr_os)))
    where
    foldingWithIndex (ToReprTop repr) _ acc nodes =
        acc <> traverseWithIndex (\i node -> do
            let id = Node.id node
            state <- Node.state node
            inputs <- Node.inputs node
            outputs <- Node.outputs node
            pure $ id
                /\ toRepr (NodeP id) state
                /\ HM.hmapWithIndex (ToReprDownI id repr) inputs
                /\ HM.hmapWithIndex (ToReprDownO id repr) outputs
        ) nodes


instance foldToReprsMap ::
    ( Semigroup (m (Array (NodeLineMap f repr)))
    , MonadEffect m
    , ToReprFoldToMapsHelper f is iks os oks repr state
    )
    => HF.FoldingWithIndex
            (ToReprTop repr)
            (Proxy sym)
            (m (Array (NodeLineMap f repr)))
            (Array (Node f state is os m))
            (m (Array (NodeLineMap f repr)))
    where
    foldingWithIndex (ToReprTop repr) _ acc nodes =
        acc <> traverseWithIndex (\i node -> do
            let (id :: NodeId f) = Node.id node
            state <- Node.state node
            inputs <- Node.inputs node
            outputs <- Node.outputs node
            pure $ id
                /\ toRepr (NodeP id) state
                /\ HF.hfoldlWithIndex (ToReprDownI id repr) (Map.empty :: Map InputR repr) inputs
                /\ HF.hfoldlWithIndex (ToReprDownO id repr) (Map.empty :: Map OutputR repr) outputs
        ) nodes


instance mapToReprDownIInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownI node_id repr)
        (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownI nodeId _) sym = toRepr (InputP nodeId $ inputP sym)


instance mapToReprDownOInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownO node_id repr)
        (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownO nodeId _) sym = toRepr (OutputP nodeId $ outputP sym)


instance foldToReprDownIInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HF.FoldingWithIndex
        (ToReprDownI node_id repr)
        (Proxy sym)
        (Map InputR repr)
        a
        (Map InputR repr)
    where
    foldingWithIndex (ToReprDownI nodeId _) sym map a =
        map # Map.insert (inputR' $ inputP sym) (toRepr (InputP nodeId $ inputP sym) a)



instance foldToReprDownOInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HF.FoldingWithIndex
        (ToReprDownO family repr)
        (Proxy sym)
        (Map OutputR repr)
        a
        (Map OutputR repr)
    where
    foldingWithIndex (ToReprDownO nodeId _) sym map a =
        map # Map.insert (outputR' $ outputP sym) (toRepr (OutputP nodeId $ outputP sym) a)



class
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI f repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO f repr) os repr_os
    ) <= ToReprFoldToRecsHelper f is iks os oks repr_is repr_os repr state
instance
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI f repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO f repr) os repr_os
    ) => ToReprFoldToRecsHelper f is iks os oks repr_is repr_os repr state

class
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HF.FoldlRecord (ToReprDownI f repr) (Map InputR repr) iks is (Map InputR repr)
    , HF.FoldlRecord (ToReprDownO f repr) (Map OutputR repr) oks os (Map OutputR repr)
    ) <= ToReprFoldToMapsHelper f is iks os oks repr state
instance
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HF.FoldlRecord (ToReprDownI f repr) (Map InputR repr) iks is (Map InputR repr)
    , HF.FoldlRecord (ToReprDownO f repr) (Map OutputR repr) oks os (Map OutputR repr)
    ) => ToReprFoldToMapsHelper f is iks os oks repr state


class
    ( RL.RowToList instances rla
    , HF.FoldlRecord
        (ToReprTop repr)
        (m (Array (NodeLineRec f state repr_is repr_os)))
        rla
        instances
        (m (Array (NodeLineRec f state repr_is repr_os)))
    ) <= FoldToReprsRec m repr rla instances f state repr_is repr_os
instance
    ( RL.RowToList instances rla
    , HF.FoldlRecord
        (ToReprTop repr)
        (m (Array (NodeLineRec f state repr_is repr_os)))
        rla
        instances
        (m (Array (NodeLineRec f state repr_is repr_os)))
    ) => FoldToReprsRec m repr rla instances f state repr_is repr_os


class
    ( RL.RowToList instances rla
    , HF.FoldlRecord
        (ToReprTop repr)
        (m (Array (NodeLineMap f repr)))
        rla
        instances
        (m (Array (NodeLineMap f repr)))
    ) <= FoldToReprsMap m rla instances f repr
instance
    ( RL.RowToList instances rla
    , HF.FoldlRecord
        (ToReprTop repr)
        (m (Array (NodeLineMap f repr)))
        rla
        instances
        (m (Array (NodeLineMap f repr)))
    ) => FoldToReprsMap m rla instances f repr
