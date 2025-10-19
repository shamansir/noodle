module Noodle.Unsafe.QuickMake.RawNode where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Data.Map (fromFoldable) as Map

import Noodle.Id (FamilyR, unsafeInletR, unsafeOutletR) as Id
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Fn.Shape.Temperament as Temp
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (make) as RawNode
import Noodle.Unsafe.QuickMake.RawFnShape (qmake_) as RawShape


qmake
    :: forall m state chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr mp
    -> m (Raw.Node state chrepr mp)
qmake familyR state toRepr =
    qmake_ familyR state toRepr Temp.defaultAlgorithm


qmake_
    :: forall m state chrepr mp
     . MonadEffect m
    => Id.FamilyR
    -> state
    -> ({ name :: String, tag :: String, value :: Maybe String } -> chrepr)
    -> Temp.Algorithm
    ->
        { inlets :: Array { name :: String, tag :: String, value :: Maybe String }
        , outlets :: Array { name :: String, tag :: String, value :: Maybe String }
        }
    -> Raw.Process state chrepr mp
    -> m (Raw.Node state chrepr mp)
qmake_ familyR state toRepr tempAlgo { inlets, outlets } process =
    RawNode.make
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


