module Noodle.Raw.Fn.Protocol
  ( Protocol
  , make
  , getState
  , getInlets, getOutlets
  , sendOut
  , sendIn
  , modifyState
  , toReprableState
  )
  where

import Prelude

import Debug as Debug

import Data.Map (Map)
import Data.Map (insert) as Map
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InletR, OutletR)

import Noodle.Raw.Fn.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from, to) as StRepr
import Noodle.Repr.ChRepr (class FromChRepr, class ToChRepr)
import Noodle.Repr.ChRepr (ensureTo, ensureFrom, wrap, unwrap) as ChRepr


type Protocol state chrepr = Generic.Protocol state (Map InletR chrepr) (Map OutletR chrepr)


make
    :: forall state chrepr m
    .  MonadEffect m
    => state
    -> Map InletR chrepr
    -> Map OutletR chrepr
    -> m (Tracker state chrepr /\ Protocol state chrepr)
make state is os =
  Generic.make state is os


getState :: forall state chrepr. Protocol state chrepr -> Effect state
getState p = p.getState unit


getInlets :: forall state chrepr. Protocol state chrepr -> Effect (Map InletR chrepr)
getInlets p = p.getInlets unit <#> Tuple.snd


getOutlets :: forall state chrepr. Protocol state chrepr -> Effect (Map OutletR chrepr)
getOutlets p = p.getOutlets unit <#> Tuple.snd


sendOut :: forall state chrepr. OutletR -> chrepr -> Protocol state chrepr -> Effect Unit
sendOut outlet = flip Generic._modifyOutlet outlet <<< Map.insert outlet


sendIn :: forall state chrepr. InletR -> chrepr -> Protocol state chrepr -> Effect Unit
sendIn inlet = flip Generic._modifyInlet inlet <<< Map.insert inlet


modifyState :: forall state chrepr. (state -> state) -> Protocol state chrepr -> Effect Unit
modifyState = Generic._modifyState


toReprableState :: forall state strepr chrepr. StRepr strepr state => Protocol state chrepr -> Protocol strepr chrepr
toReprableState =
    Generic.imapState StRepr.to StRepr.from