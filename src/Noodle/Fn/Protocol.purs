module Noodle.Fn.Protocol
  ( Protocol
  , make
  )
  where


import Data.Map (Map)
import Data.Tuple.Nested ((/\), type (/\))

import Effect.Class (class MonadEffect)

import Noodle.Id (InputR, OutputR)

import Noodle.Fn.Raw.Protocol as Raw
import Noodle.Fn.Tracker (Tracker)


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InputR repr
    -> Map OutputR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make