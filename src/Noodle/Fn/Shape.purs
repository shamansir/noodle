module Noodle.Fn.Shape where

import Prelude

import Prim.Row as Row

import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Array ((:), mapWithIndex)
-- import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TList, TNil, class MapDown, mapDown, ByReflect(..))

import Noodle.Raw.Fn.Shape (InletsShape(..), OutletsShape(..), Shape(..), InletDefR(..), OutletDefR(..)) as Raw
import Noodle.Fn.Shape.Temperament (TemperamentK(..), Hot, Cold, Temperament(..), class IsTemperament, reflectTemperament)


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


instance (IsSymbol sym, IsTemperament temp) => Reflectable (I sym temp t) (Temperament /\ String) where
    reflectType :: Proxy (I sym temp t) -> Temperament /\ String
    reflectType _ = reflectTemperament (Proxy :: _ temp) /\ reflectSymbol (Proxy :: _ sym)


instance IsSymbol sym => Reflectable (O sym t) String where
    reflectType :: Proxy (O sym t) -> String
    reflectType _ = reflectSymbol (Proxy :: _ sym)


class InletsDefs (inlets :: Inlets) where
    reflectInlets :: Proxy inlets -> Raw.InletsShape


instance MapDown ByReflect inlets Array (Temperament /\ String) => InletsDefs inlets where
    reflectInlets :: Proxy inlets -> Raw.InletsShape
    reflectInlets _ = Raw.Inlets $ mapWithIndex makeInletDef (mapDown ByReflect (Proxy :: _ inlets) :: Array (Temperament /\ String))
        where makeInletDef order (temp /\ name) = Raw.InletDefR { name, order, temp }


class OutletsDefs (outlets :: Outlets) where
    reflectOutlets :: Proxy outlets -> Raw.OutletsShape


instance MapDown ByReflect outlets Array String => OutletsDefs outlets where
    reflectOutlets :: Proxy outlets -> Raw.OutletsShape
    reflectOutlets _ = Raw.Outlets $ mapWithIndex makeOutletDef (mapDown ByReflect (Proxy :: _ outlets) :: Array String)
        where makeOutletDef order name = Raw.OutletDefR { name, order }


class ContainsAllInlets (row :: Row Type) (inlets :: Inlets) -- | inlets -> row, row -> inlets


instance ContainsAllInlets row TNil
else instance
  ( Row.Cons name din rowtail row
  , ContainsAllInlets rowtail itail
  ) => ContainsAllInlets row (I name temp din :> itail)



class ContainsAllOutlets (row :: Row Type) (outlets :: Outlets) -- | inlets -> row, row -> inlets


instance ContainsAllOutlets row TNil
else instance
  ( Row.Cons name a rowtail row
  , ContainsAllOutlets rowtail otail
  ) => ContainsAllOutlets row (O name a :> otail)


data Shape (inlets :: Inlets) (outlets :: Outlets) = Shape


reflect :: forall (inlets :: Inlets) (outlets :: Outlets). InletsDefs inlets => OutletsDefs outlets => Shape inlets outlets -> Raw.Shape
reflect _ = Raw.Shape { inlets : reflectInlets (Proxy :: _ inlets), outlets : reflectOutlets (Proxy :: _ outlets) }
