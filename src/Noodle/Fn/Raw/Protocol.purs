module Noodle.Fn.Raw.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
  , sendOut
  , sendIn
  , modifyState
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude

import Data.Map (Map)
import Data.Map (insert) as Map
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class ToRepr)
import Data.Repr (ensureTo, unwrap) as Repr
import Data.Symbol (class IsSymbol)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Noodle.Id (Input, Input', InletR, Output, Output', OutletR, inputR, inputR', outputR, outputR')

import Noodle.Fn.Raw.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state repr = Generic.Protocol state (Map InletR repr) (Map OutletR repr)


make
    :: forall state repr m
    .  MonadEffect m
    => state
    -> Map InletR repr
    -> Map OutletR repr
    -> m (Tracker state repr /\ Protocol state repr)
make = Generic.make


getState :: forall state repr. Protocol state repr -> Effect state
getState p = p.getState unit


getInputs :: forall state repr. Protocol state repr -> Effect (Map InletR repr)
getInputs p = p.getInputs unit <#> Tuple.snd


getOutputs :: forall state repr. Protocol state repr -> Effect (Map OutletR repr)
getOutputs p = p.getOutputs unit <#> Tuple.snd


sendOut :: forall state repr. OutletR -> repr -> Protocol state repr -> Effect Unit
sendOut output = flip Generic._modifyOutput output <<< Map.insert output


sendIn :: forall state repr. InletR -> repr -> Protocol state repr -> Effect Unit
sendIn input = flip Generic._modifyInput input <<< Map.insert input


modifyState :: forall state repr. (state -> state) -> Protocol state repr -> Effect Unit
modifyState = Generic._modifyState


{-
sendOut :: forall o state m dout repr. MonadEffect m => IsSymbol o => ToRepr dout repr => Protocol state repr -> Output o -> dout -> m Unit
sendOut protocol otput =
    liftEffect <<< sendOutE protocol otput


-- private?
sendOutE :: forall o state dout repr. IsSymbol o => ToRepr dout repr => Protocol state repr -> Output o -> dout -> Effect Unit
sendOutE protocol output dout =
    Generic.modifyOutput (Map.insert (outputR output) $ Repr.unwrap $ Repr.ensureTo dout) output protocol


-- private?
sendOut' :: forall o state m dout repr. MonadEffect m => IsSymbol o => ToRepr dout repr => Protocol state repr -> Output' o -> dout -> m Unit
sendOut' protocol output dout =
    Generic.modifyOutput' (Map.insert (outputR' output) $ Repr.unwrap $ Repr.ensureTo dout) output protocol


-- private?
sendOutE' :: forall o state dout repr. IsSymbol o => ToRepr dout repr => Protocol state repr -> Output' o -> dout -> Effect Unit
sendOutE' protocol output dout =
    Generic.modifyOutputE' (Map.insert (outputR' output) $ Repr.unwrap $ Repr.ensureTo dout) output protocol


-- private?
sendIn :: forall i state m din repr. MonadEffect m => IsSymbol i => ToRepr din repr => Protocol state repr -> Input i -> din -> m Unit
sendIn protocol input din =
    Generic.modifyInput (Map.insert (inputR input) $ Repr.unwrap $ Repr.ensureTo din) input protocol


-- private?
sendInE :: forall i state din repr. IsSymbol i => ToRepr din repr => Protocol state repr -> Input i -> din -> Effect Unit
sendInE protocol input din =
    Generic.modifyInputE (Map.insert (inputR input) $ Repr.unwrap $ Repr.ensureTo din) input protocol


sendIn' :: forall i state m din repr. MonadEffect m => IsSymbol i => ToRepr din repr => Protocol state repr -> Input' i -> din -> m Unit
sendIn' node i = liftEffect <<< sendInE' node i


-- private?
sendInE' :: forall i state din repr. IsSymbol i => ToRepr din repr => Protocol state repr -> Input' i -> din -> Effect Unit
sendInE' protocol input din =
    Generic.modifyInputE' (Map.insert (inputR' input) $ Repr.unwrap $ Repr.ensureTo din) input protocol


-- sendInReprE :: forall i state din repr. IsSymbol i => ToRepr din repr => Protocol state repr -> InletR -> repr -> Effect Unit
-- sendInReprE protocol input repr =
--     Generic.modifyInputE (Map.insert (inputR input) $ Repr.unwrap $ Repr.ensureTo din) input protocol

-}