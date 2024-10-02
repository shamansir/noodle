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
import Noodle.Repr (class FromRepr, class ToRepr, ensureTo, ensureFrom, wrap, unwrap)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InletR, OutletR)

import Noodle.Raw.Fn.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state repr = Generic.Protocol state (Map InletR repr) (Map OutletR repr)


make
    :: forall state repr m
    .  MonadEffect m
    => state
    -> Map InletR repr
    -> Map OutletR repr
    -> m (Tracker state repr /\ Protocol state repr)
make state is os =
  Generic.make state is os


getState :: forall state repr. Protocol state repr -> Effect state
getState p = p.getState unit


getInlets :: forall state repr. Protocol state repr -> Effect (Map InletR repr)
getInlets p = p.getInlets unit <#> Tuple.snd


getOutlets :: forall state repr. Protocol state repr -> Effect (Map OutletR repr)
getOutlets p = p.getOutlets unit <#> Tuple.snd


sendOut :: forall state repr. OutletR -> repr -> Protocol state repr -> Effect Unit
sendOut outlet = flip Generic._modifyOutlet outlet <<< Map.insert outlet


sendIn :: forall state repr. InletR -> repr -> Protocol state repr -> Effect Unit
sendIn inlet = flip Generic._modifyInlet inlet <<< Map.insert inlet


modifyState :: forall state repr. (state -> state) -> Protocol state repr -> Effect Unit
modifyState = Generic._modifyState


toReprableState :: forall state repr. FromRepr repr state => ToRepr state repr => Protocol state repr -> Protocol repr repr
toReprableState =
    Generic.imapState (ensureTo >>> unwrap) (ensureFrom <<< wrap)