module Noodle.Fn.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
  , getRecInputs, getRecOutputs
  , _sendOut, _sendOut', _unsafeSendOut
  , _sendIn, _sendIn', _unsafeSendIn
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

import Noodle.Id (class HasInput, class HasOutput, InletR, OutletR, Input, Input', Output, Output', outputR, outputR', inputR, inputR')

import Noodle.Fn.Raw.Protocol as Raw
-- TODO: import Noodle.Fn.Raw.Protocol as RawExports
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.RawToRec (toRec)

import Data.Repr (class ToRepr, class FromReprRow)
import Data.Repr (ensureFrom, ensureTo, unwrap) as Repr


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InletR repr
    -> Map OutletR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make


getState :: forall state is os repr. Protocol state is os repr -> Effect state
getState = Raw.getState


getInputs :: forall state is os repr. Protocol state is os repr -> Effect (Map InletR repr)
getInputs = Raw.getInputs


getOutputs :: forall state is os repr. Protocol state is os repr -> Effect (Map OutletR repr)
getOutputs = Raw.getOutputs


getRecInputs :: forall state is isrl os repr. FromReprRow isrl is repr => Protocol state is os repr -> Effect (Record is)
getRecInputs p = p.getInputs unit <#> Tuple.snd <#> toRec


getRecOutputs :: forall state is os osrl repr. FromReprRow osrl os repr => Protocol state is os repr -> Effect (Record os)
getRecOutputs p = p.getOutputs unit <#> Tuple.snd <#> toRec



-- private: doesn't check if output is in `os`
_sendOut :: forall o state is os dout repr. IsSymbol o => ToRepr dout repr => Output o -> dout -> Protocol state is os repr -> Effect Unit
_sendOut output = Raw.sendOut (outputR output) <<< Repr.unwrap <<< Repr.ensureTo


-- private: doesn't check if output is in `os`
_sendOut' :: forall o state is os dout repr. IsSymbol o => ToRepr dout repr => Output' o -> dout -> Protocol state is os repr -> Effect Unit
_sendOut' output = Raw.sendOut (outputR' output) <<< Repr.unwrap <<< Repr.ensureTo


-- private: doesn't check if input is in `is`
_sendIn :: forall i state is os dout repr. IsSymbol i => ToRepr dout repr => Input i -> dout -> Protocol state is os repr -> Effect Unit
_sendIn input = Raw.sendIn (inputR input) <<< Repr.unwrap <<< Repr.ensureTo


-- private: doesn't check if input is in `is`
_sendIn' :: forall i state is os dout repr. IsSymbol i => ToRepr dout repr => Input' i -> dout -> Protocol state is os repr -> Effect Unit
_sendIn' input = Raw.sendIn (inputR' input) <<< Repr.unwrap <<< Repr.ensureTo


_unsafeSendOut :: forall state is os repr. OutletR -> repr -> Protocol state is os repr -> Effect Unit
_unsafeSendOut = Raw.sendOut


_unsafeSendIn :: forall state is os repr. InletR -> repr -> Protocol state is os repr -> Effect Unit
_unsafeSendIn = Raw.sendIn
