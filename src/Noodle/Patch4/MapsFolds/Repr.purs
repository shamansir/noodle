module Noodle.Patch4.MapsFolds.Repr
  ( class FoldToReprsRec
  , class FoldToReprsMap
  , class ExtractReprs
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

import Noodle.Id (NodeId, familyP, inputP, outputP, InputR, OutputR, Input, inputR', outputR', class ListsInstances)
import Noodle.Family.Def as Family
import Noodle.Node2.Path (Path(..))
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Node2.MapsFolds.Repr as NR


{-
instance foldToReprsRec ::
    ( Semigroup (m (Array (NR.NodeLineRec f repr repr_is repr_os)))
    , MonadEffect m
    , ToReprFoldToRecsHelper f is iks os oks repr_is repr_os repr state
    )
    => HF.FoldingWithIndex
            (NR.ToReprTop repr)
            (Proxy sym)
            (m (Array (NR.NodeLineRec f repr repr_is repr_os)))
            (Array (Node f state is os m))
            (m (Array (NR.NodeLineRec f repr repr_is repr_os)))
    where
    foldingWithIndex (NR.ToReprTop repr) _ acc nodes =
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
-}


        {-
        traverseWithIndex (\i node -> do
            let id = Node.id node
            state <- Node.state node
            inputs <- Node.inputs node
            outputs <- Node.outputs node
            pure $ id
                /\ toRepr (NodeP id) state
                /\ HM.hmapWithIndex (ToReprDownI id repr) inputs
                /\ HM.hmapWithIndex (ToReprDownO id repr) outputs
        )
        -}

        {-
        toRepr (FamilyP $ familyP sym) s
            /\ HM.hmapWithIndex (ToReprDownI (familyP sym) repr) iRec
            /\ HM.hmapWithIndex (ToReprDownO (familyP sym) repr) oRec -}


class
    ( MonadEffect m
    , RL.RowToList instances rla
    , HF.FoldlRecord
        (NR.ToReprTop m repr)
        (m (Array (NR.NodeLineRec f state repr_is repr_os)))
        rla
        instances
        (m (Array (NR.NodeLineRec f state repr_is repr_os)))
    ) <= FoldToReprsRec m repr rla instances f state repr_is repr_os
instance
    ( MonadEffect m
    , RL.RowToList instances rla
    , HF.FoldlRecord
        (NR.ToReprTop m repr)
        (m (Array (NR.NodeLineRec f state repr_is repr_os)))
        rla
        instances
        (m (Array (NR.NodeLineRec f state repr_is repr_os)))
    ) => FoldToReprsRec m repr rla instances f state repr_is repr_os


class
    ( MonadEffect m
    , RL.RowToList instances rla
    , HF.FoldlRecord
        (NR.ToReprTop m repr)
        (m (Array (NR.NodeLineMap f repr)))
        rla
        instances
        (m (Array (NR.NodeLineMap f repr)))
    ) <= FoldToReprsMap m rla instances f repr
instance
    ( MonadEffect m
    , RL.RowToList instances rla
    , HF.FoldlRecord
        (NR.ToReprTop m repr)
        (m (Array (NR.NodeLineMap f repr)))
        rla
        instances
        (m (Array (NR.NodeLineMap f repr)))
    ) => FoldToReprsMap m rla instances f repr


class
    ( ListsInstances instances rla
    , HM.MapRecordWithIndex rla (NR.ToReprTop m repr) instances reprs
    )
    <= ExtractReprs
        (m :: Type -> Type)
        (rla :: RL.RowList Type)
        (instances :: Row Type)
        (reprs :: Row Type)
        (repr :: Type)
instance
    ( ListsInstances instances rla
    , HM.MapRecordWithIndex rla (NR.ToReprTop m repr) instances reprs
    )
    => ExtractReprs m rla instances reprs repr