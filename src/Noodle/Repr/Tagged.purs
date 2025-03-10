module Noodle.Repr.Tagged where

import Prelude

import Type.Proxy (Proxy)

import Noodle.Id as Id
import Noodle.Raw.Fn.Shape (ValueTag) as Shape


data ValuePath
    = Inlet  Id.FamilyR Id.InletR
    | Outlet Id.FamilyR Id.OutletR


class ValueTagged a where
    valueTag :: ValuePath -> a -> Shape.ValueTag
    acceptByTag :: Proxy a -> { incoming :: Shape.ValueTag, current :: Shape.ValueTag } -> Boolean


{-
class TypeTagged a where
    typeTag :: Proxy a -> Shape.ValueTag
-}


inlet :: Id.FamilyR -> Id.InletR -> ValuePath
inlet = Inlet


outlet :: Id.FamilyR -> Id.OutletR -> ValuePath
outlet = Outlet



inletN :: Id.NodeR -> Id.InletR -> ValuePath
inletN = Id.familyOf >>> inlet


outletN :: Id.NodeR -> Id.OutletR -> ValuePath
outletN = Id.familyOf >>> outlet