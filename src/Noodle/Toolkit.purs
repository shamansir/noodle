module Noodle.Toolkit where

import Prelude
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Data.Symbol (class IsSymbol)

import Noodle.Node (Node)

import Noodle.Toolkit.HoldsFamily (HoldsFamily, HoldsRawFamily)
import Noodle.Toolkit.Families (Families, Family, RawFamily, class FamilyExistsIn, class PutFamily)


data Toolkit (families :: Families) repr m = Toolkit (Array (HoldsFamily repr m)) (Array (HoldsRawFamily repr m))


-- register :: forall f state is os repr m. Family f state is os repr m -> Toolkit families m -> Toolkit families' m
-- registerRaw =
-- spawn :: Family -> Effect Node
-- spawnRaw