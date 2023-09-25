module Cli.Components.NodeBox.HoldsNodeState where

import Prelude

--import Noodle.Patch4 (Patch)
import Data.Maybe (Maybe(..))


-- move somewhere to Noodle / Toolkit?
class IsNodeState gstate a | a -> gstate where
    default :: a
    fromGlobal :: gstate -> Maybe a -- from Repr?


newtype HoldsNodeState = HoldsNodeState (forall r. (forall gstate state. IsNodeState gstate state => state -> gstate -> r) -> r)


holdNodeState :: forall gstate state. IsNodeState gstate state => gstate -> state -> HoldsNodeState
holdNodeState gstate state = HoldsNodeState \f -> f state gstate


withNodeState :: forall r. HoldsNodeState -> (forall gstate state. IsNodeState gstate state => state -> gstate -> r) -> r
withNodeState (HoldsNodeState f) = f


-- FIXME: these instances should not be here in the model, but else they would be orphans

instance IsNodeState a Number where
    default = 0.0
    fromGlobal = const Nothing


instance IsNodeState a Unit where
    default = unit
    fromGlobal = const Nothing