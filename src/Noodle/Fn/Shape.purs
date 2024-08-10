module Noodle.Fn.Shape where


import Prelude

import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Newtype (class Newtype, unwrap)
import Data.Array ((:))


-- | Inlet ID is used to reference corresponding inlet on a type-level by its name (e.g. as a record key)
data Inlet :: Symbol -> Type
data Inlet name = Inlet


-- | `InletR` stores rawified inlet name as String.
newtype InletR = InletR String


-- | `InletDefR` stores rawified inlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype InletDefR = InletDefR { name :: String, order :: Int, temp :: Temperament }


-- | Outlet ID is used to reference corresponding outlet on a type-level by its name (e.g. as a record key)
data Outlet :: Symbol -> Type
data Outlet name = Outlet


-- | `OutletR` stores rawified outlet name as String.
newtype OutletR = OutletR String


-- | `OutletDefR` stores rawified outlet definition, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
newtype OutletDefR = OutletDefR { name :: String, order :: Int }


instance IsSymbol name => Show (Inlet name) where
    show :: Inlet name -> String
    show Inlet = reflectSymbol (Proxy :: _ name)


instance IsSymbol name => Show (Outlet name) where
    show :: Outlet name -> String
    show Outlet = reflectSymbol (Proxy :: _ name)


instance Show InletR where
    show (InletR name) = name


instance Show OutletR where
    show (OutletR name) = name


instance Show InletDefR where
    show (InletDefR { name, order, temp }) = name <> " (" <> show order <> "," <> show temp <> " )"


instance Show OutletDefR where
    show (OutletDefR { name, order }) = name <> " (" <> show order <> ")"


inletR :: forall proxy name. IsSymbol name => proxy name -> InletR
inletR _ = InletR $ reflectSymbol (Proxy :: _ name)



outletR :: forall proxy name. IsSymbol name => proxy name -> OutletR
outletR _ = OutletR $ reflectSymbol (Proxy :: _ name)


inletName :: forall name. IsSymbol name => Inlet name -> String
inletName = const $ reflectSymbol (Proxy :: _ name)


outletName :: forall name. IsSymbol name => Outlet name -> String
outletName = const $ reflectSymbol (Proxy :: _ name)


inletRName :: InletR -> String
inletRName = unwrap


outletRName :: OutletR -> String
outletRName = unwrap


derive instance Newtype InletR _
derive instance Newtype OutletR _

derive instance Newtype InletDefR _
derive instance Newtype OutletDefR _

derive instance Eq InletR
derive instance Eq OutletR

derive instance Eq InletDefR
derive instance Eq OutletDefR

derive instance Ord InletR
derive instance Ord OutletR

derive instance Ord InletDefR
derive instance Ord OutletDefR



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


instance Show Temperament where
    show = case _ of
        Hot -> "hot"
        Cold -> "cold"


infixr 6 type ICons as /+\
infixr 6 type OCons as \+/


infixr 6 type ICons as ⟘ -- ⟂ ˔ ≀ ˄ ⚬ ≀ « ‹ ⊶
infixr 6 type OCons as ⟙ -- ˕ -- ¤ ˅ ● » › ⊷


data InletDef
foreign import data I :: Symbol -> TemperamentK -> InletDef
-- foreign import data Inlet :: Symbol -> Temperament -> Type -> InletDef


data Inlets
foreign import data ICons :: InletDef -> Inlets -> Inlets
foreign import data IS :: Inlets


data OutletDef
foreign import data O :: Symbol -> OutletDef


data Outlets
foreign import data OCons :: OutletDef -> Outlets -> Outlets
foreign import data OS :: Outlets


type TestI :: Inlets
type TestI = I "foo" Hot ⟘ I "bar" Cold ⟘ IS


type TestO :: Outlets
type TestO = O "foo" ⟙ O "bar" ⟙ OS


newtype InletsShape = Inlets (Array InletDefR)
newtype OutletsShape = Outlets (Array OutletDefR)


derive instance Newtype InletsShape _
derive instance Newtype OutletsShape _


class InletsDefs (inlets :: Inlets) where
    reflectInlets :: Proxy inlets -> Int -> InletsShape

instance InletsDefs IS where
    reflectInlets _ _ = Inlets []
else instance (IsSymbol name, IsTemperament temp, InletsDefs tail) => InletsDefs (ICons (I name temp) tail) where
    reflectInlets _ n =
        Inlets
            $ (InletDefR
                { name : reflectSymbol (Proxy :: _ name)
                , order : n
                , temp : reflectTemperament (Proxy :: _ temp)
                })
            : unwrap (reflectInlets (Proxy :: _ tail) $ n + 1)
-- else instance InletsDefs (ICons a tail) where
--     reflectInlets _ = Inlets []


class OutletsDef (outlets :: Outlets) where
    reflectOutlets :: Proxy outlets -> Int -> OutletsShape


instance OutletsDef OS where
    reflectOutlets _ _ = Outlets []
else instance (IsSymbol name, OutletsDef tail) => OutletsDef (OCons (O name) tail) where
    reflectOutlets _ n =
        Outlets
            $ (OutletDefR
                { name : reflectSymbol (Proxy :: _ name)
                , order : n
                })
            : unwrap (reflectOutlets (Proxy :: _ tail) $ n + 1)
-- else instance OutletsDef (OCons a tail) where
--     reflectOutlets _ = Outlets []


derive instance Eq Temperament
derive instance Ord Temperament