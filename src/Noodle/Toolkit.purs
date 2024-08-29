module Noodle.Toolkit where

import Prelude
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Data.Symbol (class IsSymbol)

import Noodle.Node (Node)
import Noodle.Toolkit.HoldsFamily (HoldsFamily)
import Noodle.Toolkit.Families (Families, Family, RawFamily, class FamilyExistsIn)


data Toolkit (families :: Families) repr m = Toolkit (Array (HoldsFamily repr m))