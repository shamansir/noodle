module Noodle.Fn.Shape where

import Prelude

import Prim.Row as Row

import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Reflectable (class Reflectable)
import Data.Array (mapWithIndex)
-- import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TList, TNil, class MapDown, mapDown, ByReflect(..))

import Noodle.Raw.Fn.Shape (InletR(..), OutletR(..), InletsShape(..), OutletsShape(..), Shape(..), InletDefR(..), OutletDefR(..), ValueTag) as Raw
import Noodle.Fn.Shape.Temperament (TemperamentK(..), Hot, Cold, Temperament(..), class IsTemperament, reflectTemperament)



-- | Inlet ID is used to reference corresponding inlet on a type-level by its name (e.g. as a record key)
data Inlet :: Symbol -> Type
data Inlet name = Inlet


-- | Outlet ID is used to reference corresponding outlet on a type-level by its name (e.g. as a record key)
data Outlet :: Symbol -> Type
data Outlet name = Outlet


instance IsSymbol name => Show (Inlet name) where
    show :: Inlet name -> String
    show Inlet = reflectSymbol (Proxy :: _ name)


instance IsSymbol name => Show (Outlet name) where
    show :: Outlet name -> String
    show Outlet = reflectSymbol (Proxy :: _ name)


inletName :: forall name. IsSymbol name => Inlet name -> String
inletName = const $ reflectSymbol (Proxy :: _ name)


outletName :: forall name. IsSymbol name => Outlet name -> String
outletName = const $ reflectSymbol (Proxy :: _ name)


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
    reflectInlets :: Proxy inlets -> (Raw.InletR -> Raw.ValueTag) -> Raw.InletsShape


instance MapDown ByReflect inlets Array (Temperament /\ String) => InletsDefs inlets where
    reflectInlets :: Proxy inlets -> (Raw.InletR -> Raw.ValueTag) -> Raw.InletsShape
    reflectInlets _ toTag = Raw.Inlets $ mapWithIndex makeInletDef (mapDown ByReflect (Proxy :: _ inlets) :: Array (Temperament /\ String))
        where makeInletDef order (temp /\ name) = Raw.InletDefR { name : Raw.InletR name, order, temp , tag : toTag $ Raw.InletR name}


class OutletsDefs (outlets :: Outlets) where
    reflectOutlets :: Proxy outlets -> (Raw.OutletR -> Raw.ValueTag) -> Raw.OutletsShape


instance MapDown ByReflect outlets Array String => OutletsDefs outlets where
    reflectOutlets :: Proxy outlets -> (Raw.OutletR -> Raw.ValueTag) -> Raw.OutletsShape
    reflectOutlets _ toTag = Raw.Outlets $ mapWithIndex makeOutletDef (mapDown ByReflect (Proxy :: _ outlets) :: Array String)
        where makeOutletDef order name = Raw.OutletDefR { name : Raw.OutletR name, order, tag : toTag $ Raw.OutletR name }


class ContainsAllInlets (row :: Row Type) (inlets :: Inlets) -- | inlets -> row, row -> inlets


instance ContainsAllInlets row TNil
else instance
  ( Row.Cons name din rowtail row
  , ContainsAllInlets rowtail itail
  ) => ContainsAllInlets row (I name temp din :> itail)


class ContainsAllOutlets (row :: Row Type) (outlets :: Outlets) -- | inlets -> row, row -> inlets


instance ContainsAllOutlets row TNil
else instance
  ( Row.Cons name dout rowtail row
  , ContainsAllOutlets rowtail otail
  ) => ContainsAllOutlets row (O name dout :> otail)


data Shape (inlets :: Inlets) (outlets :: Outlets) = Shape


-- FIXME: Find a way to load `Raw.Tag` from `Inlets` & `Output` defitinions for each value. We use this function by ourself, don't expose it because we can't be sure maps are corresponding to the actual values


_reflect :: forall (inlets :: Inlets) (outlets :: Outlets). InletsDefs inlets => OutletsDefs outlets => (Raw.InletR -> Raw.ValueTag) -> (Raw.OutletR -> Raw.ValueTag) -> Shape inlets outlets -> Raw.Shape
_reflect inletToTag outletToTag _ = Raw.Shape { inlets : reflectInlets (Proxy :: _ inlets) inletToTag, outlets : reflectOutlets (Proxy :: _ outlets) outletToTag }



newtype Link = Link Int