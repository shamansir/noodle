module Cli.Components.NodeBox.HoldsNodeState where

import Prelude


class IsNodeState a where
    default :: a


newtype HoldsNodeState = HoldsNodeState (forall r. (forall state. IsNodeState state => state -> r) -> r)


holdNodeState :: forall state. IsNodeState state => state -> HoldsNodeState
holdNodeState state = HoldsNodeState (_ $ state)


withNodeState :: forall r. HoldsNodeState -> (forall state. IsNodeState state => state -> r) -> r
withNodeState (HoldsNodeState f) = f


-- FIXME: these instances should not be here in the model, but else they would be orphans

instance IsNodeState Number where
    default = 0.0


instance IsNodeState Unit where
    default = unit