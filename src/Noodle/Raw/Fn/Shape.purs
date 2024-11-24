module Noodle.Raw.Fn.Shape where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

import Noodle.Fn.Shape.Temperament (Temperament)


-- | `InletR` stores rawified inlet name as String.
newtype InletR = InletR String


-- | `OutletR` stores rawified outlet name as String.
newtype OutletR = OutletR String


instance Show InletR where
    show (InletR name) = name


instance Show OutletR where
    show (OutletR name) = name


inletR :: forall proxy name. IsSymbol name => proxy name -> InletR
inletR _ = InletR $ reflectSymbol (Proxy :: _ name)


outletR :: forall proxy name. IsSymbol name => proxy name -> OutletR
outletR _ = OutletR $ reflectSymbol (Proxy :: _ name)


inletRName :: InletR -> String
inletRName = unwrap


outletRName :: OutletR -> String
outletRName = unwrap


derive instance Newtype InletR _
derive instance Newtype OutletR _

derive instance Eq InletR
derive instance Eq OutletR

derive instance Ord InletR
derive instance Ord OutletR


-- | `InletDefR` stores rawified inlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype InletDefR = InletDefR { name :: InletR, order :: Int, temp :: Temperament }


-- | `OutletDefR` stores rawified outlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype OutletDefR = OutletDefR { name :: OutletR, order :: Int }


instance Show InletDefR where
    show (InletDefR { name, order, temp }) = inletRName name <> " (" <> show order <> "," <> show temp <> " )"


instance Show OutletDefR where
    show (OutletDefR { name, order }) = outletRName name <> " (" <> show order <> ")"


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


inlets :: Shape -> Array { name :: InletR, order :: Int, temp :: Temperament }
inlets = unwrap >>> _.inlets >>> unwrap >>> map unwrap


outlets :: Shape -> Array { name :: OutletR, order :: Int }
outlets = unwrap >>> _.outlets >>> unwrap >>> map unwrap


make ::
    { inlets  :: Array { name :: InletR, order :: Int, temp :: Temperament }
    , outlets :: Array { name :: OutletR, order :: Int }
    }
    -> Shape
make { inlets, outlets } =
    Shape
        { inlets : Inlets $ InletDefR <$> inlets
        , outlets : Outlets $ OutletDefR <$> outlets
        }