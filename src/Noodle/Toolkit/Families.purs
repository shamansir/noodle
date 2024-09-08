module Noodle.Toolkit.Families where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Repr (class ToReprRow)

import Type.Proxy (Proxy(..))

import Noodle.Id (Family(..), InletR, OutletR, familyR, inletR, outletR) as Id
import Noodle.Fn (Fn)
import Noodle.Fn (make) as Fn
import Noodle.Fn.Process (Process)
import Noodle.Fn.Shape (Shape, Inlets, Outlets, class ContainsAllInlets, class ContainsAllOutlets, class InletsDefs, class OutletsDefs)
import Noodle.Fn.Shape (reflect) as Shape
import Noodle.Node (Node)
import Noodle.Node (makeWithFn_) as Node

import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Toolkit.Family (Family)
import Noodle.Raw.FromToRec as ReprCnv


infixr 6 type FCons as //


data Families
foreign import data FCons :: FamilyDef -> Families -> Families
foreign import data FNil :: Families


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef


class FamilyExistsIn (family :: FamilyDef) (families :: Families)
instance FamilyExistsIn family (FCons family tail)
else instance (FamilyExistsIn family tail) => FamilyExistsIn family (FCons skipfamily tail)



class PutFamily (family :: FamilyDef) (families :: Families) (families' :: Families) | families -> families'


instance PutFamily family FNil (FCons family FNil)
else instance PutFamily family (FCons some tail) (FCons family (FCons some tail))