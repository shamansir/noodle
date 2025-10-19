module Noodle.Unsafe.QuickMake.RawFamily where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Data.Map (fromFoldable) as Map

import Noodle.Id (unsafeInletR, unsafeOutletR, FamilyR) as Id
import Noodle.Fn.Shape.Temperament as Temp
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make) as RawFamily
import Noodle.Unsafe.QuickMake.RawFnShape (qmake_) as RawShape


qmake
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr m
    -> Raw.Family state chrepr m
qmake familyR state toRepr =
    qmake_ familyR state toRepr Temp.defaultAlgorithm


qmake_
    :: forall state chrepr m
     . Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    -> Temp.Algorithm
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr m
    -> Raw.Family state chrepr m
qmake_ familyR state toRepr tempAlgo { inlets, outlets } process =
    RawFamily.make
        familyR
        state
        (RawShape.qmake_ tempAlgo
            { inlets : cutValue <$> inlets
            , outlets : cutValue <$> outlets }
        )
        ( Map.fromFoldable $ iConvert <$> inlets )
        ( Map.fromFoldable $ oConvert <$> outlets )
        process
    where
        cutValue { name, tag } = { name, tag }
        iConvert { name, tag, value } = Id.unsafeInletR  name /\ toRepr { name, tag, value }
        oConvert { name, tag, value } = Id.unsafeOutletR name /\ toRepr { name, tag, value }
