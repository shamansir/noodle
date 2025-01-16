module Noodle.Raw.Fn.Protocol
  ( Protocol
  , make, make'
  , getState
  , getInlets, getOutlets
  , sendOut
  , sendIn
  , modifyState
  , toReprableState
  )
  where

import Prelude

import Data.Map (Map)
import Data.Map (insert) as Map
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (fromMaybe)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InletR, OutletR)

import Noodle.Raw.Fn.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (accept) as ViC
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (ensureFrom, to) as StRepr


type Protocol state chrepr = Generic.Protocol state (Map InletR (ValueInChannel chrepr)) (Map OutletR (ValueInChannel chrepr))


make
    :: forall state chrepr m
    .  MonadEffect m
    => state
    -> Map InletR  chrepr -- FIXME: initial values should always be accepted
    -> Map OutletR chrepr -- FIXME: initial values should always be accepted
    -> m (Tracker state chrepr /\ Protocol state chrepr)
make state is os =
  make' state (ViC.accept <$> is) (ViC.accept <$> os)


make'
    :: forall state chrepr m
    .  MonadEffect m
    => state
    -> Map InletR  (ValueInChannel chrepr) -- FIXME: initial values should always be accepted
    -> Map OutletR (ValueInChannel chrepr) -- FIXME: initial values should always be accepted
    -> m (Tracker state chrepr /\ Protocol state chrepr)
make' = Generic.make


getState :: forall state chrepr. Protocol state chrepr -> Effect state
getState p = p.getState unit


getInlets :: forall state chrepr. Protocol state chrepr -> Effect (Map InletR (ValueInChannel chrepr))
getInlets p = p.getInlets unit <#> Tuple.snd


getOutlets :: forall state chrepr. Protocol state chrepr -> Effect (Map OutletR (ValueInChannel chrepr))
getOutlets p = p.getOutlets unit <#> Tuple.snd


sendOut :: forall state chrepr. OutletR -> ValueInChannel chrepr -> Protocol state chrepr -> Effect Unit
sendOut outlet = flip Generic._modifyOutlet outlet <<< Map.insert outlet


sendIn :: forall state chrepr. InletR -> ValueInChannel chrepr -> Protocol state chrepr -> Effect Unit
sendIn inlet = flip Generic._modifyInlet inlet <<< Map.insert inlet


modifyState :: forall state chrepr. (state -> state) -> Protocol state chrepr -> Effect Unit
modifyState = Generic._modifyState


toReprableState :: forall state strepr chrepr. HasFallback state => StRepr state strepr => Protocol state chrepr -> Protocol strepr chrepr
toReprableState =
    Generic.imapState StRepr.to StRepr.ensureFrom