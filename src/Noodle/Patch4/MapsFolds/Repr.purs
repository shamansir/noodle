module Noodle.Patch4.MapsFolds.Repr
  ( class FoldToReprsRec
  , class FoldToReprsMap
  , class ExtractReprs
  )
  where


import Effect.Class (class MonadEffect)
--import Data.FoldableWithIndex (foldlWithIndex)
import Prim.RowList as RL


import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Noodle.Id (class ListsInstances)
import Noodle.Node2.MapsFolds.Flatten (NodeLineRec, NodeLineMap) as NR
import Noodle.Node2.MapsFolds.Repr (ToReprTop) as NR


class FoldToReprsRec :: forall k. (Type -> Type) -> k -> RL.RowList Type -> Row Type -> Symbol -> Type -> Row Type -> Row Type -> Constraint
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
        (m (Array (NR.NodeLineMap repr)))
        rla
        instances
        (m (Array (NR.NodeLineMap repr)))
    ) <= FoldToReprsMap m rla instances repr
instance
    ( MonadEffect m
    , RL.RowToList instances rla
    , HF.FoldlRecord
        (NR.ToReprTop m repr)
        (m (Array (NR.NodeLineMap repr)))
        rla
        instances
        (m (Array (NR.NodeLineMap repr)))
    ) => FoldToReprsMap m rla instances repr


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