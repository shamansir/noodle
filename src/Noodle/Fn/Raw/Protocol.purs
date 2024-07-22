module Noodle.Fn.Raw.Protocol
  ( Protocol
  , make
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InputR, OutputR)

import Noodle.Fn.Raw.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state repr = Generic.Protocol state (Map InputR repr) (Map OutputR repr)


make
    :: forall state repr m
    .  MonadEffect m
    => state
    -> Map InputR repr
    -> Map OutputR repr
    -> m (Tracker state repr /\ Protocol state repr)
make = Generic.make