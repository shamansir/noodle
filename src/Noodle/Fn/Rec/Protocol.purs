module Noodle.Fn.Rec.Protocol
  ( Protocol
  , Tracker
  , make
  , inputs, outputs
  , lastInput, lastOutput
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.SProxy (reflect, reflect')

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)

import Noodle.Id (InputR, OutputR)
import Noodle.Stateful (class StatefulM)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state (is :: Row Type) (os :: Row Type) = Generic.Protocol state (Record is) (Record os)
type Tracker  state (is :: Row Type) (os :: Row Type) = Generic.Tracker  state (Record is) (Record os)


type PreUpdatesRow  state is os = Generic.PreUpdatesRow  state (Record is) (Record os)
type PostUpdatesRow state is os = Generic.PostUpdatesRow state (Record is) (Record os)
type FocusedUpdate  state is os = Generic.FocusedUpdate  state (Record is) (Record os)


make
    :: forall state (is :: Row Type) (os :: Row Type) m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os /\ Protocol state is os)
make = Generic.make


inputs :: forall state is os. Tracker state is os -> Effect (Record is)
inputs = Generic.inputs


outputs :: forall state is os. Tracker state is os -> Effect (Record os)
outputs = Generic.outputs


lastInput :: forall state is os. Tracker state is os -> Effect (Maybe InputR)
lastInput = Generic.lastInput


lastOutput :: forall state is os. Tracker state is os -> Effect (Maybe OutputR)
lastOutput = Generic.lastOutput