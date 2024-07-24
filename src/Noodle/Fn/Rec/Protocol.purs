module Noodle.Fn.Rec.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude (unit, (<#>))

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
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


getState :: forall state is os. Protocol state is os -> Effect state
getState p = p.getState unit


getInputs :: forall state is os. Protocol state is os -> Effect (Record is)
getInputs p = p.getInputs unit <#> Tuple.snd


getOutputs :: forall state is os. Protocol state is os -> Effect (Record os)
getOutputs p = p.getOutputs unit <#> Tuple.snd