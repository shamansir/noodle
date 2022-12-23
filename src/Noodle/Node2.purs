module Noodle.Node2
  where


import Prelude

import Prim.RowList as RL

import Data.Newtype (class Newtype, unwrap)

import Data.Array as Array
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List)
import Data.List (length, filter) as List

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as Process
import Noodle.Fn2.Protocol (Protocol, ProtocolS)
import Noodle.Fn2.Protocol as Protocol
import Noodle.Fn2.Flow (keysToInputs, keysToOutputs, InputId, OutputId, Input, Output, inputIdToString, outputIdToString) as Fn
import Noodle.Fn2 (Fn)
import Noodle.Fn2 (_in, _out, inputsShape, outputsShape) as Fn

import Record.Extra (keys, class Keys) as Record
import Signal.Channel (Channel)


type Family = String


type UID = Int


data Node state (is :: Row Type) (os :: Row Type) m = Node (Family /\ UID) (ProtocolS state is os Channel m) (Fn state is os m)


-- TODO: implement ToFn
-- TODO: implement HasInputs
-- TODO: implement HasOutputs

{-
class ToFn a state is os where
    toFn :: forall m. a -> Fn state is os m


-- TODO: extend to HasInputs, HasOutputs with getAtInput, getAtOutput, updateInputs, updateOutputs, ...
class (RL.RowToList is g, Record.Keys g) <= HasInputs is g

class (RL.RowToList os g, Record.Keys g) <= HasOutputs os g
-}


make :: forall state is os m. (Family /\ UID) -> state -> Record is -> Record os -> Fn state is os m -> m (Node state is os m)
make = Node >>> pure -- FIXME:


_in :: Fn.InputId -> String
_in = Fn._in


_out :: Fn.OutputId -> String
_out = Fn._out


{-}
mapM :: forall state is os m m'. (m ~> m') -> Fn state is os m -> Fn state is os m'
mapM f (Node id protocol processM) = Fn name state is os $ Process.mapMM f processM


imapState :: forall state state' is os m. (state -> state') -> (state' -> state) -> Fn state is os m -> Fn state' is os m
imapState f g (Fn name state is os processM) = Fn name (f state) is os $ Process.imapMState f g processM
-}

{- Running -}


run :: forall state is os m. MonadRec m => MonadEffect m => Node state is os m -> m Unit
run (Node _ protocolS fn) = pure unit -- FIXME


run' :: forall state is os m. MonadRec m => MonadEffect m => state -> Record is -> Record os -> Node state is os m -> m Unit
run' state is os (Node _ protocolS fn) = pure unit -- FIXME
    -- _ <- Process.runM protocol process
    -- nextState <- protocol.getState unit
    -- nextInputs <- protocol.getInputs unit
    -- nextOutputs <- protocol.getOutputs unit
    -- pure $ nextState /\ nextInputs /\ nextOutputs


{- Get information about the function -}


family :: forall state is os m. Node state is os m -> Family
family (Node (family /\ _) _ _) = family


uid :: forall state is os m. Node state is os m -> UID
uid (Node (_ /\ uid) _ _) = uid


state :: forall state is os m. Node state is os m -> m state
state (Fn _ state _ _ _) = state


inputs :: forall state is os m. Node state is os m -> m (Record is)
inputs (Fn _ _ inputs _ _) = inputs


outputs :: forall state is os m. Node state is os m -> m (Record os)
outputs (Fn _ _ _ outputs _) = outputs


get :: forall state is os m. Node state is os m -> m ( state /\ Record is /\ Record os )
get fn = state fn /\ inputs fn /\ outputs fn


set :: forall state is os m. ( state /\ Record is /\ Record os ) -> Node state is os m -> m (Node state is os m)
set ( state /\ inputs /\ outputs ) (Fn name _ _ _ process) = Fn name state inputs outputs process


-- TODO: getAtInput, getAtOutput, updateInputs, updateOutputs, updateState ...



inputsShape :: forall state (is :: Row Type) os m g. RL.RowToList is g => Record.Keys g => Node state is os m -> List Fn.InputId
inputsShape (Node _ _ fn) = Fn.inputsShape fn


outputsShape :: forall state is (os :: Row Type) m g. RL.RowToList os g => Record.Keys g => Fn state is os m -> List Fn.OutputId
outputsShape (Node _ _ fn) = Fn.outputsShape fn


-- TODO: mapRecord


shape
    :: forall state (is :: Row Type) (os :: Row Type) m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Fn state is os m
    -> List Fn.InputId /\ List Fn.OutputId
shape fn = inputsShape fn /\ outputsShape fn


dimensions
    :: forall state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Fn state is os m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => (Fn.InputId -> Boolean)
    -> (Fn.OutputId -> Boolean)
    -> Fn state is os m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)


{-
findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os m -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}