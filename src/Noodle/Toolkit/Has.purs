module Noodle.Toolkit.Has where


import Type.Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Data.Repr (class FromToReprRow, class ReadWriteRepr)

import Noodle.Id (Family, FamilyR, Input, Output) as Node
import Noodle.Patch.Has (class HasInstancesOf)
import Noodle.Id (class HasInputsAt, class HasOutputsAt, class HasInputsAt', class HasOutputsAt') as Has
import Noodle.Family.Def as Family
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Node.MapsFolds.Repr as NMF


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
    , HasFamilyDef f fs families (Family.Def state is os m)
    , HasInstancesOf f iis instances (Array (Noodle.Node f state is os m))
    , Has.HasInputsAt is rli
    , Has.HasInputsAt' is rli
    , Has.HasOutputsAt os rlo
    , Has.HasOutputsAt' os rlo
    ) <= HasNodesOf families instances f state fs iis rli is rlo os m

instance
    ( IsSymbol f
    , HasFamilyDef f fs families (Family.Def state is os m)
    , HasInstancesOf f iis instances (Array (Noodle.Node f state is os m))
    , Has.HasInputsAt is rli
    , Has.HasInputsAt' is rli
    , Has.HasOutputsAt os rlo
    , Has.HasOutputsAt' os rlo
    ) => HasNodesOf families instances f state fs iis rli is rlo os m


class
    ( ReadWriteRepr repr
    , HasNodesOf families instances f state fs iis rli is rlo os m
    , NMF.ToReprHelper m f is rli os rlo repr_is repr_os repr state
    , NMF.ToReprFoldToMapsHelper f is rli os rlo repr state
    , Node.NodeBoundKeys Node.I rli Node.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O rlo Node.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    --    => Node.NodeBoundKeys Node.I rli Node.Input f state is os m x
    --    => Node.NodeBoundKeys Node.O rlo Node.Output f state is os m x
    , FromToReprRow rli is repr
    , FromToReprRow rlo os repr
    ) <= HasReprableNodesOf families instances repr f state fs iis rli is rlo os repr_is repr_os m

instance
    ( ReadWriteRepr repr
    , HasNodesOf families instances f state fs iis rli is rlo os m
    , NMF.ToReprHelper m f is rli os rlo repr_is repr_os repr state
    , NMF.ToReprFoldToMapsHelper f is rli os rlo repr state
    , Node.NodeBoundKeys Node.I rli Node.Input f state is os m (Node.HoldsInputInNodeMRepr m repr)
    , Node.NodeBoundKeys Node.O rlo Node.Output f state is os m (Node.HoldsOutputInNodeMRepr m repr)
    --    => Node.NodeBoundKeys Node.I rli Node.Input f state is os m x
    --    => Node.NodeBoundKeys Node.O rlo Node.Output f state is os m x
    , FromToReprRow rli is repr
    , FromToReprRow rlo os repr
    ) => HasReprableNodesOf families instances repr f state fs iis rli is rlo os repr_is repr_os m

-- class ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) <= HasNodesOf f state fs iis is os m

-- instance ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) => HasNodesOf f state fs iis is os m


-- TODO: Move `ListsFamilies` here