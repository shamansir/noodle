module Noodle.Toolkit.Has where


import Type.Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Data.Repr (class FromToReprRow, class ReadWriteRepr)

import Noodle.Id (Family, FamilyR, Input, Output) as Node
import Noodle.Patch.Has (class HasInstancesOf) as PHas
import Noodle.Id (class HasInputsAt, class HasOutputsAt, class HasOrderedInputKeysAt, class HasOrderedOutputKeysAt) as Has
import Noodle.Family.Def as Family
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Node.MapsFolds.Repr as NMF
import Noodle.Node.HoldsNodeState (class IsNodeState)

import Cli.Components.NodeBox.HasBody (class HasCliBody, class HasCliCustomSize) -- FIXME: must be located somewhere in generic UI


class HasFamilyDef :: forall k. Symbol -> Row k -> Row k -> k -> Constraint
class
    ( IsSymbol f
    , Row.Cons f x families' families
    )
    <= HasFamilyDef f families' families x -- FIXME: use newtype
instance
    ( IsSymbol f
    , Row.Cons f x families' families
    )
    => HasFamilyDef f families' families x -- FIXME: use newtype


class HasFamilyDef' :: forall k. Symbol -> Row k -> Row k -> k -> Constraint
class
    ( Row.Cons f x families' families
    )
    <= HasFamilyDef' f families' families x -- FIXME: use newtype
instance
    ( Row.Cons f x families' families
    )
    => HasFamilyDef' f families' families x -- FIXME: use newtype


class
    ( IsSymbol f
    , HasFamilyDef f families' families (Family.Def state is os m)
    , PHas.HasInstancesOf f instances' instances (Array (Noodle.Node f state is os m))
    , Has.HasInputsAt is isrl
    , Has.HasOrderedInputKeysAt is isrl
    , Has.HasOutputsAt os osrl
    , Has.HasOrderedOutputKeysAt os osrl
    ) <= HasNodesOf families' families instances' instances f state isrl is osrl os m

instance
    ( IsSymbol f
    , HasFamilyDef f families' families (Family.Def state is os m)
    , PHas.HasInstancesOf f instances' instances (Array (Noodle.Node f state is os m))
    , Has.HasInputsAt is isrl
    , Has.HasOrderedInputKeysAt is isrl
    , Has.HasOutputsAt os osrl
    , Has.HasOrderedOutputKeysAt os osrl
    ) => HasNodesOf families' families instances' instances f state isrl is osrl os m


class
    ( HasNodesOf families' families instances' instances f state isrl is osrl os m
    , ReadWriteRepr repr
    , NMF.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , NMF.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , Node.NodeBoundKeys Node.I isrl Node.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Node.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    --    => Node.NodeBoundKeys Node.I isrl Node.Input f state is os m x
    --    => Node.NodeBoundKeys Node.O osrl Node.Output f state is os m x
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    ) <= HasReprableNodesOf families' families instances' instances repr f state isrl is osrl os repr_is repr_os m

instance
    ( HasNodesOf families' families instances' instances f state isrl is osrl os m
    , ReadWriteRepr repr
    , NMF.ToReprHelper m f is isrl os osrl repr_is repr_os repr state
    , NMF.ToReprFoldToMapsHelper f is isrl os osrl repr state
    , Node.NodeBoundKeys Node.I isrl Node.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O osrl Node.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    --    => Node.NodeBoundKeys Node.I isrl Node.Input f state is os m x
    --    => Node.NodeBoundKeys Node.O osrl Node.Output f state is os m x
    , FromToReprRow isrl is repr
    , FromToReprRow osrl os repr
    ) => HasReprableNodesOf families' families instances' instances repr f state isrl is osrl os repr_is repr_os m


class HasReprableRenderableNodesOf :: forall k. (Symbol -> k) -> Type -> Row Type -> Row Type -> Row Type -> Row Type -> Type -> Symbol -> Type -> RL.RowList Type -> Row Type -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
    ( HasReprableNodesOf families' families instances' instances repr f state isrl is osrl os repr_is repr_os m
    , HasCliBody (x f) (Noodle.Node f state is os m) state m
    , HasCliCustomSize (x f) (Noodle.Node f state is os m)
    , IsNodeState gstate state
    ) <= HasReprableRenderableNodesOf x gstate families' families instances' instances repr f state isrl is osrl os repr_is repr_os m


instance
    ( HasReprableNodesOf families' families instances' instances repr f state isrl is osrl os repr_is repr_os m
    , HasCliBody (x f) (Noodle.Node f state is os m) state m
    , HasCliCustomSize (x f) (Noodle.Node f state is os m)
    , IsNodeState gstate state
    ) => HasReprableRenderableNodesOf x gstate families' families instances' instances repr f state isrl is osrl os repr_is repr_os m

-- class ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , PHas.HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) <= HasNodesOf f state fs iis is os m

-- instance ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , PHas.HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) => HasNodesOf f state fs iis is os m


-- TODO: Move `ListsFamilies` here