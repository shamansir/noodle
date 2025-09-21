module Noodle.Raw.Fn.Shape where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Data.Array as Array
import Data.Maybe (Maybe, maybe)

import Noodle.Fn.Shape.Temperament (Temperament)
import Noodle.Fn.Shape.Temperament (Temperament(..)) as Temp


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


unsafeInletR :: String -> InletR
unsafeInletR = wrap


unsafeOutletR :: String -> OutletR
unsafeOutletR = wrap


-- | `InletDefR` stores rawified inlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype InletDefR = InletDefR { name :: InletR, order :: Int, temp :: Temperament, tag :: ValueTag }


-- | `OutletDefR` stores rawified outlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype OutletDefR = OutletDefR { name :: OutletR, order :: Int, tag :: ValueTag }


instance Show InletDefR where
    show (InletDefR { name, order, temp, tag }) = inletRName name <> " (" <> show order <> "," <> show temp <> "," <> show tag <> ")"


instance Show OutletDefR where
    show (OutletDefR { name, order, tag }) = outletRName name <> " (" <> show order <> "," <> show tag <> ")"


derive instance Newtype InletDefR _
derive instance Newtype OutletDefR _

derive instance Eq InletDefR
derive instance Eq OutletDefR


derive instance Ord InletDefR
derive instance Ord OutletDefR


newtype InletsShape  = Inlets  (Array InletDefR)
newtype OutletsShape = Outlets (Array OutletDefR)


derive instance Newtype InletsShape _
derive instance Newtype OutletsShape _
derive newtype instance Show InletsShape
derive newtype instance Show OutletsShape
derive newtype instance Eq InletsShape
derive newtype instance Eq OutletsShape


newtype Shape = Shape { inlets :: InletsShape, outlets :: OutletsShape }


derive instance Newtype Shape _
derive newtype instance Show Shape
derive newtype instance Eq Shape


make ::
    { inlets  :: Array { name :: InletR,  order :: Int, temp :: Temperament, tag :: ValueTag }
    , outlets :: Array { name :: OutletR, order :: Int, tag :: ValueTag }
    }
    -> Shape
make { inlets, outlets } =
    Shape
        { inlets  : Inlets  $ InletDefR  <$> inlets
        , outlets : Outlets $ OutletDefR <$> outlets
        }


empty :: Shape
empty = make { inlets : [], outlets : [] }


inlets :: Shape -> Array { name :: InletR, order :: Int, temp :: Temperament, tag :: ValueTag }
inlets = unwrap >>> _.inlets >>> unwrap >>> map unwrap


outlets :: Shape -> Array { name :: OutletR, order :: Int, tag :: ValueTag }
outlets = unwrap >>> _.outlets >>> unwrap >>> map unwrap


inletsCount :: Shape -> Int
inletsCount = inlets >>> Array.length


outletsCount :: Shape -> Int
outletsCount = outlets >>> Array.length


inletAtIndex :: Int -> Shape -> Maybe { name :: InletR, order :: Int, temp :: Temperament, tag :: ValueTag }
inletAtIndex idx = inlets >>> Array.filter (_.order >>> (_ == idx)) >>> Array.head


outletAtIndex :: Int -> Shape -> Maybe { name :: OutletR, order :: Int, tag :: ValueTag }
outletAtIndex idx = outlets >>> Array.filter (_.order >>> (_ == idx)) >>> Array.head


hasHotInlets :: Shape -> Boolean
hasHotInlets = unwrap >>> _.inlets >>> unwrap >>> Array.any _isHotInlet


findInletDef :: InletR -> Shape -> Maybe InletDefR
findInletDef inletR = unwrap >>> _.inlets >>> unwrap >>> Array.find (unwrap >>> _.name >>> (_ == inletR))


findOutletDef :: OutletR -> Shape -> Maybe OutletDefR
findOutletDef ouletR = unwrap >>> _.outlets >>> unwrap >>> Array.find (unwrap >>> _.name >>> (_ == ouletR))


isHotInlet :: InletR -> Shape -> Maybe Boolean
isHotInlet inletR = findInletDef inletR >>> map _isHotInlet


_isHotInlet :: InletDefR -> Boolean
_isHotInlet = unwrap >>> _.temp >>> (_ == Temp.Hot)


temperamentOf :: InletR -> Shape -> Maybe Temperament
temperamentOf inletR = findInletDef inletR >>> map (unwrap >>> _.temp)


indexOfInlet :: InletR -> Shape -> Maybe Int
indexOfInlet inletR = findInletDef inletR >>> map (unwrap >>> _.order)


indexOfOutlet :: OutletR -> Shape -> Maybe Int
indexOfOutlet outletR = findOutletDef outletR >>> map (unwrap >>> _.order)


-- the types below are sometimes helpful to distinguich inlet index from outlet index in function calls


newtype InletIndex = InletIndex Int
newtype OutletIndex = OutletIndex Int


derive newtype instance Eq InletIndex
derive instance Ord InletIndex

derive instance Eq OutletIndex
derive instance Ord OutletIndex


newtype ValueTag = ValueTag String


derive instance Newtype ValueTag _
derive newtype instance Eq ValueTag
derive newtype instance Ord ValueTag
instance Show ValueTag where show (ValueTag tag) = "<" <> tag <> ">"


tagAs :: String -> ValueTag
tagAs = ValueTag


failed :: ValueTag
failed = ValueTag "FAIL-X"


tagOfInlet :: InletR -> Shape -> Maybe ValueTag
tagOfInlet inletR = findInletDef inletR >>> map (unwrap >>> _.tag)


tagOfOutlet :: OutletR -> Shape -> Maybe ValueTag
tagOfOutlet outletR = findOutletDef outletR >>> map (unwrap >>> _.tag)
