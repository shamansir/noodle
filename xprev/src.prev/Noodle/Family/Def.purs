module Noodle.Family.Def where

import Prelude

import Data.List (List)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))

import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL

import Noodle.Id
import Noodle.Fn (Fn)
import Noodle.Fn as Fn



newtype Def state (is :: Row Type) (os :: Row Type) repr (m :: Type -> Type) =
    Def (state /\ Record is /\ Record os /\ Fn state is os repr m)


instance HasInputsAt is isrl => HasInputs is isrl (Def state is os repr m) where
    inputs :: Def state is os repr m -> List InputR
    inputs = Fn.inputsShape <<< fn


instance HasOutputsAt os osrl => HasOutputs os osrl (Def state is os repr m) where
    outputs :: Def state is os repr m -> List OutputR
    outputs = Fn.outputsShape <<< fn


fn :: forall state is os repr m. Def state is os repr m -> Fn state is os repr m
fn (Def (_ /\ _ /\ _ /\ fn_)) = fn_


def :: forall state is os repr m. state -> Record is -> Record os -> Fn state is os repr m -> Def state is os repr m
def state is os fn_ = Def (state /\ is /\ os /\ fn_)


-- inputsFromDef :: forall rl state (is :: Row Type) os m. Keys rl => RL.RowToList is rl => Def state is os m -> List InputR
-- inputsFromDef _ = InputR <$> Record.keys (Proxy :: Proxy is)