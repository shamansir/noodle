module Noodle.Patch.Is where


import Data.Repr (class FromToReprRow)

import Prim.RowList as RL
import Record.Extra (class Keys) as Record

import Noodle.Id (Input, Output, class HasInput, class HasOutput, class HasInputsAt, class HasOutputsAt) as Id
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Patch.Has (class HasInstancesOf) as Has
import Noodle.Node.MapsFolds.Repr (class ToReprHelper, class ToReprFoldToMapsHelper) as R
import Noodle.Node.HoldsNodeState (class IsNodeState)

import Cli.Components.NodeBox.HasBody (class HasBody', class HasCustomSize) -- FIXME: should not be located in the Cli module but instead some general Ui module

class IsNodeInPatch :: forall k. k -> Row Type -> Row Type -> RL.RowList Type -> Symbol -> Type -> Row Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> (Type -> Type) -> Constraint
class
    ( Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rli
    , Record.Keys rli
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    ) <= IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m


instance
    ( Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rli
    , Record.Keys rli
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    ) => IsNodeInPatch gstate instances instances' rli f state is os isrl osrl m


class IsNodeInPatch' :: (Symbol -> Type) -> Type -> Row Type -> Row Type -> RL.RowList Type -> Symbol -> Type -> Row Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> Constraint
class
    ( Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rli
    , Record.Keys rli
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasBody' (x f) (Node f state is os m) state m
    , HasCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) <= IsNodeInPatch' x gstate instances instances' rli f state is os isrl osrl repr_is repr_os repr m


instance
    ( Has.HasInstancesOf f instances' instances (Array (Node f state is os m))
    , RL.RowToList instances rli
    , Record.Keys rli
    , Id.HasInputsAt is isrl
    , Id.HasOutputsAt os osrl
    , R.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , R.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    , Node.NodeBoundKeys Node.I isrl Id.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Id.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    , HasBody' (x f) (Node f state is os m) state m
    , HasCustomSize (x f) (Node f state is os m)
    , IsNodeState gstate state
    ) => IsNodeInPatch' x gstate instances instances' rli f state is os isrl osrl repr_is repr_os repr m


class LinkStartInPatch :: forall k. Symbol -> Symbol -> Type -> Type -> Row Type -> Row Type -> Row Type -> k -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
    ( Has.HasInstancesOf fA insA ins (Array (Node fA stateA isA osA m))
    , Id.HasOutput oA doutA osA' osA
    ) <= LinkStartInPatch fA oA doutA stateA isA osA osA' gstate ins insA m


class LinkEndInPatch :: forall k. Symbol -> Symbol -> Type -> Type -> Row Type -> Row Type -> Row Type -> k -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
    ( Has.HasInstancesOf fB insB ins (Array (Node fB stateB isB osB m))
    , Id.HasInput iB dinB isB' isB
    ) <= LinkEndInPatch fB iB dinB stateB isB isB' osB gstate ins insB m