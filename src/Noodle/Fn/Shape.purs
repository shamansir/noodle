module Noodle.Fn.Shape where


import Prelude

import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList as RL

import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Newtype (class Newtype, unwrap, wrap)
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


infixr 6 type ICons as +> -- /+\
infixr 6 type OCons as :> -- <+ -- \+/


infixr 6 type ICons as ⟘ -- ⟂ ˔ ≀ ˄ ⚬ ≀ « ‹ ⊶
infixr 6 type OCons as ⟙ -- ˕ -- ¤ ˅ ● » › ⊷


data InletDef a = I { default :: a }
foreign import data I :: forall (t :: Type). Symbol -> TemperamentK -> t -> InletDef t
-- FIXME: don't include types of the inlet, we don't use it in any way, only to check conformance with default values
-- foreign import data Inlet :: Symbol -> Temperament -> Type -> InletDef


data Inlets
foreign import data ICons :: forall (t :: Type). InletDef t -> Inlets -> Inlets
foreign import data IS :: Inlets


data OutletDef a = O { default :: a }
foreign import data O :: forall (t :: Type). Symbol -> t -> OutletDef t
-- FIXME: don't include types of the outlet, we don't use it in any way, only to check conformance with default values


data Outlets
foreign import data OCons :: forall (t :: Type). OutletDef t -> Outlets -> Outlets
foreign import data OS :: Outlets


newtype InletsShape = Inlets (Array InletDefR)
newtype OutletsShape = Outlets (Array OutletDefR)


derive instance Newtype InletsShape _
derive instance Newtype OutletsShape _


class InletsDefs (inlets :: Inlets) where
    reflectInlets :: Proxy inlets -> Int -> InletsShape

instance InletsDefs IS where
    reflectInlets _ _ = Inlets []
else instance (IsSymbol name, IsTemperament temp, InletsDefs tail) => InletsDefs (ICons (I name temp din) tail) where
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


class OutletsDefs (outlets :: Outlets) where
    reflectOutlets :: Proxy outlets -> Int -> OutletsShape


instance OutletsDefs OS where
    reflectOutlets _ _ = Outlets []
else instance (IsSymbol name, OutletsDefs tail) => OutletsDefs (OCons (O name dout) tail) where
    reflectOutlets _ n =
        Outlets
            $ (OutletDefR
                { name : reflectSymbol (Proxy :: _ name)
                , order : n
                })
            : unwrap (reflectOutlets (Proxy :: _ tail) $ n + 1)


class HasInlet (name :: Symbol) (din :: Type) (inlets :: Inlets)
instance HasInlet name din (ICons (I name temp din) tail)
else instance (HasInlet name din tail) => HasInlet name din (ICons (I skipname skiptemp skipdin) tail)


class ContainsAllInlets (row :: Row Type) (inlets :: Inlets) -- | inlets -> row, row -> inlets


instance ContainsAllInlets row IS
else instance
  ( Row.Cons name a rowtail row
  , ContainsAllInlets rowtail itail
  ) => ContainsAllInlets row (ICons (I name temp a) itail)



-- class ContainsAllInlets (row :: Row Type) (inlets :: Inlets)
-- instance ContainsAllInlets RL.Nil IS
-- else instance
--     ( ContainsInlet name din (RL.Cons rname rdin rtail)
--     , ContainsAllInlets rtail tail
--     ) => ContainsAllInlets (RL.Cons rname rdin rtail) (ICons (I name temp din) tail)


class HasOutlet (name :: Symbol) (dout :: Type) (inlets :: Outlets)
instance HasOutlet name dout (OCons (O name dout) tail)
else instance (HasOutlet name dout tail) => HasOutlet name dout (OCons (O skipname skipdout) tail)


class ContainsAllOutlets (row :: Row Type) (outlets :: Outlets) -- | inlets -> row, row -> inlets


instance ContainsAllOutlets row OS
else instance
  ( Row.Cons name a rowtail row
  , ContainsAllOutlets rowtail otail
  ) => ContainsAllOutlets row (OCons (O name a) otail)


derive instance Eq Temperament
derive instance Ord Temperament


data Shape (inlets :: Inlets) (outlets :: Outlets) = Shape
newtype Raw = Raw { inlets :: InletsShape, outlets :: OutletsShape }


derive instance Newtype Raw _


reflect :: forall (inlets :: Inlets) (outlets :: Outlets). InletsDefs inlets => OutletsDefs outlets => Shape inlets outlets -> Raw
reflect _ = Raw { inlets : reflectInlets (Proxy :: _ inlets) 0, outlets : reflectOutlets (Proxy :: _ outlets) 0 }


inlets :: Raw -> Array { name :: String, order :: Int, temp :: Temperament }
inlets = unwrap >>> _.inlets >>> unwrap >>> map unwrap


outlets :: Raw -> Array { name :: String, order :: Int }
outlets = unwrap >>> _.outlets >>> unwrap >>> map unwrap


-- isInlet :: forall (inlet :: InletDef) (name :: Symbol) (din :: Type). IsInlet name din inlet => Proxy name -> Proxy din -> Proxy inlet -> Unit
-- isInlet _ _ _ = unit


-- hasInlet :: forall (name :: Symbol) (din :: Type) (inlets :: Inlets). HasInlet name din inlets => Proxy name -> Proxy din -> Proxy inlets -> Unit
-- hasInlet _ _ _ = unit


-- inletsMatch :: forall (inlets :: Inlets) irl irow. RL.RowToList irow irl => InletsMatch irl inlets => Proxy inlets -> Record irow -> Unit
-- inletsMatch _ _ = unit


-- outletsMatch :: forall (outlets :: Outlets) orl orow. RL.RowToList orow orl => OutletsMatch orl outlets => Proxy outlets -> Record orow -> Unit
-- outletsMatch _ _ = unit


-- inletsMap :: Raw -> Map String { name :: String, order :: Int }