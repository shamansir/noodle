module Noodle.Fn.Rec.Protocol
  ( Protocol
  , make
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Data.Tuple.Nested ((/\), type (/\))

import Effect.Class (class MonadEffect)

import Noodle.Fn.Rec.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state (is :: Row Type) (os :: Row Type) = Generic.Protocol state (Record is) (Record os)


make
    :: forall state (is :: Row Type) (os :: Row Type) m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os /\ Protocol state is os)
make = Generic.make