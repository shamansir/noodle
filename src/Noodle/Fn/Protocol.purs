module Noodle.Fn.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
  , getRecInputs, getRecOutputs
  , _sendOut, _sendOut', _sendOutE, _sendOutE'
  , _sendIn, _sendIn', _sendInE, _sendInE'
  )
  where


import Prelude

import Prim.RowList as RL

import Data.Map (Map)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (class HasInput, class HasOutput, InputR, OutputR, Input, Input', Output, Output')

import Noodle.Fn.Raw.Protocol as Raw
-- TODO: import Noodle.Fn.Raw.Protocol as RawExports
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.RawToRec (toRec)

import Data.Repr (class ToRepr, class FromReprRow)


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InputR repr
    -> Map OutputR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make


getState :: forall state is os repr. Protocol state is os repr -> Effect state
getState = Raw.getState


getInputs :: forall state is os repr. Protocol state is os repr -> Effect (Map InputR repr)
getInputs = Raw.getInputs


getOutputs :: forall state is os repr. Protocol state is os repr -> Effect (Map OutputR repr)
getOutputs = Raw.getOutputs


getRecInputs :: forall state is isrl os repr. FromReprRow isrl is repr => Protocol state is os repr -> Effect (Record is)
getRecInputs p = p.getInputs unit <#> Tuple.snd <#> toRec


getRecOutputs :: forall state is os osrl repr. FromReprRow osrl os repr => Protocol state is os repr -> Effect (Record os)
getRecOutputs p = p.getOutputs unit <#> Tuple.snd <#> toRec



-- private: doesn't check if output is in `os`
_sendOut :: forall o state  is os m dout repr. MonadEffect m => IsSymbol o => ToRepr dout repr => Protocol state is os repr -> Output o -> dout -> m Unit
_sendOut = Raw.sendOut


-- private: doesn't check if output is in `os`
_sendOutE :: forall o state is os dout repr. IsSymbol o => ToRepr dout repr => Protocol state is os repr -> Output o -> dout -> Effect Unit
_sendOutE = Raw.sendOutE


-- private: doesn't check if output is in `os`
_sendOut' :: forall o state is os m dout repr. MonadEffect m => IsSymbol o => ToRepr dout repr => Protocol state is os repr -> Output' o -> dout -> m Unit
_sendOut' = Raw.sendOut'


-- private: doesn't check if output is in `os`
_sendOutE' :: forall o state is os dout repr. IsSymbol o => ToRepr dout repr => Protocol state is os repr -> Output' o -> dout -> Effect Unit
_sendOutE' = Raw.sendOutE'


-- private: doesn't check if input is in `is`
_sendIn :: forall i state is os m din repr. MonadEffect m => IsSymbol i => ToRepr din repr => Protocol state is os repr -> Input i -> din -> m Unit
_sendIn = Raw.sendIn


-- private: doesn't check if input is in `is`
_sendInE :: forall i state is os din repr. IsSymbol i => ToRepr din repr => Protocol state is os repr -> Input i -> din -> Effect Unit
_sendInE = Raw.sendInE


-- private: doesn't check if input is in `is`
_sendIn' :: forall i state is os m din repr. MonadEffect m => IsSymbol i =>  ToRepr din repr => Protocol state is os repr -> Input' i -> din -> m Unit
_sendIn' = Raw.sendIn'


-- private: doesn't check if input is in `is`
_sendInE' :: forall i state is os din repr. IsSymbol i => ToRepr din repr => Protocol state is os repr -> Input' i -> din -> Effect Unit
_sendInE' = Raw.sendInE'
