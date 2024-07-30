module Noodle.Fn.Rec.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
  , sendOut, sendOut'
  , sendIn, sendIn'
  , modifyState
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude

import Record (set) as Record

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)
import Data.SProxy (proxify)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (Input, Input', Output, Output', class HasInput, class HasOutput, inputR, inputR', outputR, outputR')

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


sendOut :: forall o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Output o -> dout -> Protocol state is os -> Effect Unit
sendOut output dout =
    Generic._modifyOutput (Record.set (proxify output) dout) $ outputR output


sendOut' :: forall o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Output' o -> dout -> Protocol state is os -> Effect Unit
sendOut' output dout =
    Generic._modifyOutput (Record.set (proxify output) dout) $ outputR' output


sendIn :: forall i state is is' os m din. MonadEffect m => HasInput i din is' is => Input i -> din -> Protocol state is os -> Effect Unit
sendIn input din =
    Generic._modifyInput (Record.set (proxify input) din) $ inputR input


-- private?
sendIn' :: forall i state is is' os din. IsSymbol i => HasInput i din is' is => Input' i -> din -> Protocol state is os -> Effect Unit
sendIn' input din =
    Generic._modifyInput (Record.set (proxify input) din) $ inputR' input


modifyState :: forall state is os. (state -> state) -> Protocol state is os -> Effect Unit
modifyState = Generic._modifyState
