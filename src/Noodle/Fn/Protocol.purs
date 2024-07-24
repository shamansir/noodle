module Noodle.Fn.Protocol
  ( Protocol
  , make
  , getState
  , getInputs, getOutputs
  , getRecInputs, getRecOutputs
  , sendOut, sendOut', sendOutE, sendOutE'
  , sendIn, sendIn', sendInE, sendInE'
  )
  where


import Prelude

import Prim.RowList as RL

import Data.Map (Map)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

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


getRecInputs :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Protocol state is os repr -> Effect (Record is)
getRecInputs p = p.getInputs unit <#> Tuple.snd <#> toRec


getRecOutputs :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Protocol state is os repr -> Effect (Record os)
getRecOutputs p = p.getOutputs unit <#> Tuple.snd <#> toRec



sendOut :: forall o state  is os osrl m dout repr. MonadEffect m => HasOutput o dout osrl os =>  ToRepr dout repr => Protocol state is os repr -> Output o -> dout -> m Unit
sendOut = Raw.sendOut


-- private?
sendOutE :: forall o state is os osrl dout repr. HasOutput o dout osrl os => ToRepr dout repr => Protocol state is os repr -> Output o -> dout -> Effect Unit
sendOutE = Raw.sendOutE


-- private?
sendOut' :: forall o state is os osrl m dout repr. MonadEffect m => HasOutput o dout osrl os => ToRepr dout repr => Protocol state is os repr -> Output' o -> dout -> m Unit
sendOut' = Raw.sendOut'


-- private?
sendOutE' :: forall o state is os osrl dout repr. HasOutput o dout osrl os => ToRepr dout repr => Protocol state is os repr -> Output' o -> dout -> Effect Unit
sendOutE' = Raw.sendOutE'


-- private?
sendIn :: forall i state is isrl os m din repr. MonadEffect m => HasInput i din isrl is => ToRepr din repr => Protocol state is os repr -> Input i -> din -> m Unit
sendIn = Raw.sendIn


-- private?
sendInE :: forall i state is isrl os din repr. HasInput i din isrl is => ToRepr din repr => Protocol state is os repr -> Input i -> din -> Effect Unit
sendInE = Raw.sendInE


sendIn' :: forall i state is isrl os m din repr. MonadEffect m => HasInput i din isrl is => ToRepr din repr => Protocol state is os repr -> Input' i -> din -> m Unit
sendIn' = Raw.sendIn'


-- private?
sendInE' :: forall i state is isrl os din repr. HasInput i din isrl is => ToRepr din repr => Protocol state is os repr -> Input' i -> din -> Effect Unit
sendInE' = Raw.sendInE'
