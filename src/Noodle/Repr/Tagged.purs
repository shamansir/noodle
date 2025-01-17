module Noodle.Repr.Tagged where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype)

import Noodle.Id as Id
import Noodle.Raw.Fn.Shape (Tag) as Shape


data Path
    = Inlet  Id.FamilyR Id.InletR
    | Outlet Id.FamilyR Id.OutletR


class Tagged a where
    tag :: Path -> a -> Shape.Tag


{-
class TypeTagged a where
    typeTag :: Proxy a -> Shape.Tag
-}


inlet :: Id.FamilyR -> Id.InletR -> Path
inlet = Inlet


outlet :: Id.FamilyR -> Id.OutletR -> Path
outlet = Outlet



inletN :: Id.NodeR -> Id.InletR -> Path
inletN = Id.familyOf >>> inlet


outletN :: Id.NodeR -> Id.OutletR -> Path
outletN = Id.familyOf >>> outlet