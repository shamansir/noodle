module Toolkit.Test.Repr where

import Prelude

import Data.Repr as R
import Data.Maybe (Maybe(..))

import Noodle.Node.MapsFolds.Repr (class HasRepr)


data AlwaysUnitRepr = Unit_


instance Show AlwaysUnitRepr
    where
        show Unit_ = "Unit"


instance HasRepr Unit AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr String AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr Int AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr Boolean AlwaysUnitRepr where toRepr _ _ = Unit_


instance R.HasFallback AlwaysUnitRepr where fallback = Unit_


instance R.ToRepr Int AlwaysUnitRepr where toRepr _ = Just $ R.Repr Unit_
instance R.ToRepr String AlwaysUnitRepr where toRepr _ = Just $ R.Repr Unit_
instance R.FromRepr AlwaysUnitRepr Int where fromRepr _ = Just 0
instance R.FromRepr AlwaysUnitRepr String where fromRepr _ = Just ""
