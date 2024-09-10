module Noodle.Fn.Shape where


import Prelude

import Prim.Row as Row

import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Array ((:))

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TList, TNil, TCons)

import Noodle.Raw.Fn.Shape (InletsShape(..), OutletsShape(..), Shape(..), InletDefR(..), OutletDefR(..)) as Raw
import Noodle.Fn.Shape.Temperament (TemperamentK, class IsTemperament, reflectTemperament)


-- | Inlet ID is used to reference corresponding inlet on a type-level by its name (e.g. as a record key)
data Inlet :: Symbol -> Type
data Inlet name = Inlet


-- | `InletR` stores rawified inlet name as String.
newtype InletR = InletR String


-- | Outlet ID is used to reference corresponding outlet on a type-level by its name (e.g. as a record key)
data Outlet :: Symbol -> Type
data Outlet name = Outlet


-- | `OutletR` stores rawified outlet name as String.
newtype OutletR = OutletR String


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

derive instance Eq InletR
derive instance Eq OutletR

derive instance Ord InletR
derive instance Ord OutletR


-- data InletDef t = I { default :: t }
data InletDef
-- foreign import data I :: forall (t :: Type). Symbol -> TemperamentK -> t -> InletDef t
foreign import data I :: forall (t :: Type). Symbol -> TemperamentK -> t -> InletDef
-- FIXME: don't include types of the inlet, we don't use it in any way, only to check conformance with default values
-- foreign import data Inlet :: Symbol -> Temperament -> Type -> InletDef


type Inlets = TList InletDef


-- data OutletDef t = O { default :: t }
data OutletDef
-- foreign import data O :: forall (t :: Type). Symbol -> t -> OutletDef t
foreign import data O :: forall (t :: Type). Symbol -> t -> OutletDef
-- FIXME: don't include types of the outlet, we don't use it in any way, only to check conformance with default values


type Outlets = TList OutletDef


class InletsDefs (inlets :: Inlets) where
    reflectInlets :: Proxy inlets -> Int -> Raw.InletsShape

instance InletsDefs TNil where
    reflectInlets _ _ = Raw.Inlets []
else instance (IsSymbol name, IsTemperament temp, InletsDefs tail) => InletsDefs (I name temp din :> tail) where
    reflectInlets _ n =
        Raw.Inlets
            $ (Raw.InletDefR
                { name : reflectSymbol (Proxy :: _ name)
                , order : n
                , temp : reflectTemperament (Proxy :: _ temp)
                })
            : unwrap (reflectInlets (Proxy :: _ tail) $ n + 1)
-- else instance InletsDefs (ICons a tail) where
--     reflectInlets _ = Inlets []


class OutletsDefs (outlets :: Outlets) where
    reflectOutlets :: Proxy outlets -> Int -> Raw.OutletsShape


instance OutletsDefs TNil where
    reflectOutlets _ _ = Raw.Outlets []
else instance (IsSymbol name, OutletsDefs tail) => OutletsDefs (O name dout :> tail) where
    reflectOutlets _ n =
        Raw.Outlets
            $ (Raw.OutletDefR
                { name : reflectSymbol (Proxy :: _ name)
                , order : n
                })
            : unwrap (reflectOutlets (Proxy :: _ tail) $ n + 1)


class HasInlet (name :: Symbol) (din :: Type) (inlets :: Inlets) -- FIXME: same as typelevel Membership test
instance HasInlet name din (I name temp din :> tail)
else instance (HasInlet name din tail) => HasInlet name din (I skipname skiptemp skipdin :> tail)


class ContainsAllInlets (row :: Row Type) (inlets :: Inlets) -- | inlets -> row, row -> inlets


instance ContainsAllInlets row TNil
else instance
  ( Row.Cons name a rowtail row
  , ContainsAllInlets rowtail itail
  ) => ContainsAllInlets row (I name temp a :> itail)



-- class ContainsAllInlets (row :: Row Type) (inlets :: Inlets)
-- instance ContainsAllInlets RL.Nil IS
-- else instance
--     ( ContainsInlet name din (RL.Cons rname rdin rtail)
--     , ContainsAllInlets rtail tail
--     ) => ContainsAllInlets (RL.Cons rname rdin rtail) (ICons (I name temp din) tail)


class HasOutlet (name :: Symbol) (dout :: Type) (inlets :: Outlets) -- FIXME: same as typelevel Membership test
instance HasOutlet name dout (O name dout :> tail)
else instance (HasOutlet name dout tail) => HasOutlet name dout (O skipname skipdout :> tail)


class ContainsAllOutlets (row :: Row Type) (outlets :: Outlets) -- | inlets -> row, row -> inlets


instance ContainsAllOutlets row TNil
else instance
  ( Row.Cons name a rowtail row
  , ContainsAllOutlets rowtail otail
  ) => ContainsAllOutlets row (O name a :> otail)


data Shape (inlets :: Inlets) (outlets :: Outlets) = Shape


reflect :: forall (inlets :: Inlets) (outlets :: Outlets). InletsDefs inlets => OutletsDefs outlets => Shape inlets outlets -> Raw.Shape
reflect _ = Raw.Shape { inlets : reflectInlets (Proxy :: _ inlets) 0, outlets : reflectOutlets (Proxy :: _ outlets) 0 }



-- isInlet :: forall (inlet :: InletDef) (name :: Symbol) (din :: Type). IsInlet name din inlet => Proxy name -> Proxy din -> Proxy inlet -> Unit
-- isInlet _ _ _ = unit


-- hasInlet :: forall (name :: Symbol) (din :: Type) (inlets :: Inlets). HasInlet name din inlets => Proxy name -> Proxy din -> Proxy inlets -> Unit
-- hasInlet _ _ _ = unit


-- inletsMatch :: forall (inlets :: Inlets) irl irow. RL.RowToList irow irl => InletsMatch irl inlets => Proxy inlets -> Record irow -> Unit
-- inletsMatch _ _ = unit


-- outletsMatch :: forall (outlets :: Outlets) orl orow. RL.RowToList orow orl => OutletsMatch orl outlets => Proxy outlets -> Record orow -> Unit
-- outletsMatch _ _ = unit


-- inletsMap :: Raw -> Map String { name :: String, order :: Int }