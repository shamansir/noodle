module Noodle.Node2.MapsFolds.Repr
  ( Repr(..)
  , ToReprTop(..)
  , ToReprDownI, ToReprDownO
  , class HasRepr
  , toRepr
  , class ToReprHelper, class ToReprFoldToMapsHelper
  , nodeToRepr, nodeToMapRepr
  , subscribeReprChanges, subscribeReprMapChanges
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
import Data.SOrder (SOrder)
import Data.SOrder as SOrder
import Prim.RowList as RL

import Signal (Signal, (~>))
import Signal as Signal


import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Noodle.Id (InputR, NodeId, OutputR, inputP', inputR', nodeIdR, outputP', outputR')
import Noodle.Node2.MapsFolds.Flatten (NodeLineRec, NodeLineMap)
import Noodle.Node2.Path (InNode(..))
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2.Protocol (ChangeFocus(..))


-- TODO: move to a separate top-level `Repr` module, may be find a way to keep non-top constructors hidden
data ToReprTop :: forall k. (Type -> Type) -> k -> Type
data ToReprTop m repr = ToReprTop (Repr repr)
data ToReprDownI :: forall k. Symbol -> k -> Type
data ToReprDownI f repr = ToReprDownI (NodeId f) SOrder (Repr repr)
data ToReprDownO :: forall k. Symbol -> k -> Type
data ToReprDownO f repr = ToReprDownO (NodeId f) SOrder (Repr repr)


data Repr :: forall k. k -> Type
data Repr a = Repr



class HasRepr a repr where
    toRepr :: forall f i o. InNode f i o -> a -> repr -- include Repr as kind here?



nodeToRepr'
    :: forall m is os repr state f iks repr_is oks repr_os
     . MonadEffect m
    => ToReprHelper m f is iks os oks repr_is repr_os repr state
    => ToReprTop m repr
    -> Node f state is os m
    -> m (NodeLineRec f repr repr_is repr_os)
nodeToRepr' (ToReprTop repr) node = do
    let
        (id :: NodeId f) = Node.id node
        iorder = Node.inputsOrder node
        oorder = Node.outputsOrder node
    state <- Node.state node
    inputs <- Node.inputs node
    outputs <- Node.outputs node
    pure $ id
        /\ toRepr (NodeP id) state
        /\ HM.hmapWithIndex (ToReprDownI id iorder repr) inputs
        /\ HM.hmapWithIndex (ToReprDownO id oorder repr) outputs


nodeToRepr
    :: forall m is os repr state f iks repr_is oks repr_os
     . MonadEffect m
    => ToReprHelper m f is iks os oks repr_is repr_os repr state
    => Proxy m
    -> Repr repr
    -> Node f state is os m
    -> m (NodeLineRec f repr repr_is repr_os)
nodeToRepr _ repr =
    nodeToRepr' $ ToReprTop repr


nodeToMapRepr'
    :: forall m is os repr state f iks oks
     . MonadEffect m
    => ToReprFoldToMapsHelper f is iks os oks repr state
    => ToReprTop m repr
    -> Node f state is os m
    -> m (NodeLineMap repr)
nodeToMapRepr' (ToReprTop repr) node = do
    let
        (id :: NodeId f) = Node.id node
        iorder = Node.inputsOrder node
        oorder = Node.outputsOrder node
    state <- Node.state node
    inputs <- Node.inputs node
    outputs <- Node.outputs node
    pure $ nodeIdR id
        /\ toRepr (NodeP id) state
        /\ HF.hfoldlWithIndex (ToReprDownI id iorder repr) (Map.empty :: Map InputR repr) inputs
        /\ HF.hfoldlWithIndex (ToReprDownO id oorder repr) (Map.empty :: Map OutputR repr) outputs


nodeToMapRepr
    :: forall m is os repr state f iks oks
     . MonadEffect m
    => ToReprFoldToMapsHelper f is iks os oks repr state
    => Proxy m
    -> Repr repr
    -> Node f state is os m
    -> m (NodeLineMap repr)
nodeToMapRepr _ repr =
    nodeToMapRepr' $ ToReprTop repr


subscribeReprChanges'
    :: forall f state is iks os oks m repr repr_is repr_os
     . ToReprHelper m f is iks os oks repr_is repr_os repr state
    => ToReprTop m repr
    -> Node f state is os m
    -> Signal (ChangeFocus /\ NodeLineRec f repr repr_is repr_os)
subscribeReprChanges' (ToReprTop repr) node =
    let
        (id :: NodeId f) = Node.id node
        iorder = Node.inputsOrder node
        oorder = Node.outputsOrder node
        toReprs (chFocus /\ state /\ inputs /\ outputs) =
            chFocus
            /\ id
            /\ toRepr (NodeP id) state
            /\ HM.hmapWithIndex (ToReprDownI id iorder repr) inputs
            /\ HM.hmapWithIndex (ToReprDownO id oorder repr) outputs
    in toReprs <$> Node.subscribeChanges node


subscribeReprChanges
    :: forall f state is iks os oks m repr repr_is repr_os
     . ToReprHelper m f is iks os oks repr_is repr_os repr state
    => Repr repr
    -> Node f state is os m
    -> Signal (ChangeFocus /\ NodeLineRec f repr repr_is repr_os)
subscribeReprChanges repr =
    subscribeReprChanges' $ ToReprTop repr


subscribeReprMapChanges'
    :: forall f state is iks os oks m repr
     . ToReprFoldToMapsHelper f is iks os oks repr state
    => ToReprTop m repr
    -> Node f state is os m
    -> Signal (ChangeFocus /\ NodeLineMap repr)
subscribeReprMapChanges' (ToReprTop repr) node =
    let
        (id :: NodeId f) = Node.id node
        iorder = Node.inputsOrder node
        oorder = Node.outputsOrder node
        toReprs (chFocus /\ state /\ inputs /\ outputs) =
            chFocus
            /\ nodeIdR id
            /\ toRepr (NodeP id) state
            /\ HF.hfoldlWithIndex (ToReprDownI id iorder repr) (Map.empty :: Map InputR repr) inputs
            /\ HF.hfoldlWithIndex (ToReprDownO id oorder repr) (Map.empty :: Map OutputR repr) outputs
    in toReprs <$> Node.subscribeChanges node


subscribeReprMapChanges
    :: forall f state is iks os oks m repr
     . ToReprFoldToMapsHelper f is iks os oks repr state
    => Repr repr
    -> Node f state is os m
    -> Signal (ChangeFocus /\ NodeLineMap repr)
subscribeReprMapChanges repr =
    subscribeReprMapChanges' $ ToReprTop repr


instance toReprTopInstance ::
    ( MonadEffect m
    , ToReprHelper m f is iks os oks repr_is repr_os repr state
    -- , HM.MapRecordWithIndex iks (ToReprDownI f3 repr4)
    --                                          is5
    --                                          repr_is6
    ) =>
    HM.MappingWithIndex
        (ToReprTop m repr)
        (Proxy f)
        (Array (Node f state is os m))
        (m (Array (NodeLineRec f repr repr_is repr_os))) -- FIXME becomes orphan instance when put in Patch4.MapsFolds.Repr
    where
    mappingWithIndex (ToReprTop repr) fsym =
        traverseWithIndex $ const $ nodeToRepr' (ToReprTop repr)


instance toReprDownIInstance ::
    ( IsSymbol i
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownI node_id repr)
        (Proxy i)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownI node_id iorder _) isym =
        toRepr (InputP node_id $ inputP' iorder isym)


instance toReprDownOInstance ::
    ( IsSymbol o
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownO node_id repr)
        (Proxy o)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownO node_id oorder _) osym =
        toRepr (OutputP node_id $ outputP' oorder osym)


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
    foldingWithIndex (ToReprDownI nodeId iorder _) sym map a = -- ORDER!
        map # Map.insert (inputR' input') (toRepr (InputP nodeId input') a)
        where input' = inputP' iorder sym



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
    foldingWithIndex (ToReprDownO nodeId oorder _) sym map a = -- ORDER!
        map # Map.insert (outputR' output') (toRepr (OutputP nodeId output') a)
        where output' = outputP' oorder sym


class
    ( MonadEffect m
    , IsSymbol sym
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI sym repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO sym repr) os repr_os
    ) <= ToReprHelper m sym is iks os oks repr_is repr_os repr state
instance
    ( MonadEffect m
    , IsSymbol sym
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI sym repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO sym repr) os repr_os
    ) => ToReprHelper m sym is iks os oks repr_is repr_os repr state

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


instance foldToReprsMap ::
    ( Semigroup (m (Array (NodeLineMap repr)))
    , MonadEffect m
    , ToReprFoldToMapsHelper f is iks os oks repr state
    )
    => HF.FoldingWithIndex
            (ToReprTop m repr)
            (Proxy sym)
            (m (Array (NodeLineMap repr)))
            (Array (Node f state is os m))
            (m (Array (NodeLineMap repr)))
    where
    foldingWithIndex (ToReprTop repr) _ acc nodes =
        acc <> traverseWithIndex (\i node -> do
            let
                (id :: NodeId f) = Node.id node
                iorder = Node.inputsOrder node
                oorder = Node.outputsOrder node
            state <- Node.state node
            inputs <- Node.inputs node
            outputs <- Node.outputs node
            pure $ nodeIdR id
                /\ toRepr (NodeP id) state
                /\ HF.hfoldlWithIndex (ToReprDownI id iorder repr) (Map.empty :: Map InputR repr) inputs
                /\ HF.hfoldlWithIndex (ToReprDownO id oorder repr) (Map.empty :: Map OutputR repr) outputs
        ) nodes