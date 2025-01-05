module Noodle.Fn.Shape.Temperament where

import Prelude

import Type.Proxy (Proxy)
import Data.Reflectable (class Reflectable)


data TemperamentK


foreign import data Hot :: TemperamentK
foreign import data Cold :: TemperamentK


-- | `Temperament` is stored in Inlet and so it can be `Hot` or `Cold`:
-- |
-- | * _Hot_ means that receiving any new data at it triggers the re-computation of the Node function;
-- | * _Cold_ means that receiving any new data just keeps it held there and node function waits for receiving a data from another hot inlet to trigger;
data Temperament
    = Hot
    | Cold


class IsTemperament :: TemperamentK -> Constraint
class IsTemperament temp where
  reflectTemperament :: Proxy temp -> Temperament


instance IsTemperament Hot where
    reflectTemperament _ = Hot

instance IsTemperament Cold where
    reflectTemperament _ = Cold


instance Reflectable Hot Temperament where
    reflectType :: Proxy Hot -> Temperament
    reflectType = reflectTemperament


instance Reflectable Cold Temperament where
    reflectType :: Proxy Cold -> Temperament
    reflectType = reflectTemperament


instance Show Temperament where
    show = case _ of
        Hot -> "hot"
        Cold -> "cold"


derive instance Eq Temperament
derive instance Ord Temperament


data Algorithm
    = AllHot
    | HotHeadColdTail
    | AllCold


defaultAlgorithm :: Algorithm
defaultAlgorithm = AllHot


infixr 6 byIndex as <+->


byIndex :: Algorithm -> Int -> Temperament
byIndex AllHot _ = Hot
byIndex AllCold _ = Cold
byIndex HotHeadColdTail 0 = Hot
byIndex HotHeadColdTail _ = Cold
