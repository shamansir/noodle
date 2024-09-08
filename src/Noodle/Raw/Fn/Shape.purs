module Noodle.Raw.Fn.Shape where

import Prelude

import Data.Newtype (class Newtype, unwrap)

import Noodle.Fn.Shape.Temperament (Temperament)

-- | `InletDefR` stores rawified inlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype InletDefR = InletDefR { name :: String, order :: Int, temp :: Temperament }


-- | `OutletDefR` stores rawified outlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype OutletDefR = OutletDefR { name :: String, order :: Int }


instance Show InletDefR where
    show (InletDefR { name, order, temp }) = name <> " (" <> show order <> "," <> show temp <> " )"


instance Show OutletDefR where
    show (OutletDefR { name, order }) = name <> " (" <> show order <> ")"


derive instance Newtype InletDefR _
derive instance Newtype OutletDefR _

derive instance Eq InletDefR
derive instance Eq OutletDefR


derive instance Ord InletDefR
derive instance Ord OutletDefR


newtype InletsShape = Inlets (Array InletDefR)
newtype OutletsShape = Outlets (Array OutletDefR)


derive instance Newtype InletsShape _
derive instance Newtype OutletsShape _


newtype Shape = Shape { inlets :: InletsShape, outlets :: OutletsShape }


derive instance Newtype Shape _


inlets :: Shape -> Array { name :: String, order :: Int, temp :: Temperament }
inlets = unwrap >>> _.inlets >>> unwrap >>> map unwrap


outlets :: Shape -> Array { name :: String, order :: Int }
outlets = unwrap >>> _.outlets >>> unwrap >>> map unwrap


make ::
    { inlets :: Array { name :: String, order :: Int, temp :: Temperament }
    , outlets :: Array { name :: String, order :: Int }
    }
    -> Shape
make { inlets, outlets } =
    Shape
        { inlets : Inlets $ InletDefR <$> inlets
        , outlets : Outlets $ OutletDefR <$> outlets
        }
