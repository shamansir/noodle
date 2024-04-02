module Noodle.Patch.Is where


import Data.Repr (class FromToReprRow)

import Prim.RowList as RL
import Record.Extra (class Keys) as Record

import Noodle.Id (Input, Output, class HasInput, class HasOutput, class HasInputsAt, class HasOutputsAt) as Id
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Patch.Has (class HasInstancesOf) as PHas
import Noodle.Node.MapsFolds.Repr (class ToReprHelper, class ToReprFoldToMapsHelper) as R
import Noodle.Node.HoldsNodeState (class IsNodeState)

import Cli.Components.NodeBox.HasBody (class HasCliBody, class HasCliCustomSize) -- FIXME: should not be located in the Cli module but instead some general Ui module

class IsNodeInPatch :: forall k. k -> Row Type -> Row Type -> RL.RowList Type -> Symbol -> Type -> Row Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> (Type -> Type) -> Constraint
class
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rlins
    , Record.Keys rlins
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    ) <= IsNodeInPatch gstate instances' instances rlins f state is os isrl osrl m


instance
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rlins
    , Record.Keys rlins
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    ) => IsNodeInPatch gstate instances' instances rlins f state is os isrl osrl m


class IsReprableNodeInPatch :: (Symbol -> Type) -> Type -> Row Type -> Row Type -> RL.RowList Type -> Symbol -> Type -> Row Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> Constraint
class
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rlins
    , Record.Keys rlins
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasCliBody (x f) (Node f state is os m) state m
    , HasCliCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) <= IsReprableNodeInPatch x gstate instances' instances rlins f state is os isrl osrl repr_is repr_os repr m


instance
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rlins
    , Record.Keys rlins
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasCliBody (x f) (Node f state is os m) state m
    , HasCliCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) => IsReprableNodeInPatch x gstate instances' instances rlins f state is os isrl osrl repr_is repr_os repr m


class IsReprableRenderableNodeInPatch :: (Symbol -> Type) -> Type -> Row Type -> Row Type -> RL.RowList Type -> Symbol -> Type -> Row Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> Constraint
class
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasCliBody (x f) (Node f state is os m) state m
    , HasCliCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) <= IsReprableRenderableNodeInPatch x gstate instances' instances rlins f state is os isrl osrl repr_is repr_os repr m


instance
    ( PHas.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasCliBody (x f) (Node f state is os m) state m
    , HasCliCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) => IsReprableRenderableNodeInPatch x gstate instances' instances rlins f state is os isrl osrl repr_is repr_os repr m


class LinkStartInPatch :: forall k. Symbol -> Symbol -> Type -> Type -> Row Type -> Row Type -> Row Type -> k -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
    ( PHas.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    , Id.HasOutput oA doutA osA' osA
    ) <= LinkStartInPatch fA oA doutA stateA isA osA osA' gstate ins insA m


class LinkEndInPatch :: forall k. Symbol -> Symbol -> Type -> Type -> Row Type -> Row Type -> Row Type -> k -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
    ( PHas.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    , Id.HasInput iB dinB isB' isB
    ) <= LinkEndInPatch fB iB dinB stateB isB isB' osB gstate ins insB m