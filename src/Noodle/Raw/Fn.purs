module Noodle.Raw.Fn where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Map (Map)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (FnName, InletR, OutletR)

import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (runM) as RawProcess
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Raw.Fn.Protocol (getState, getInlets, getOutlets) as RawProtocol
import Noodle.Repr (class HasFallback)


data Fn state repr (m :: Type -> Type) = Fn FnName (Raw.Process state repr m) -- TODO: move to separate module


make :: forall state repr m. FnName -> Raw.Process state repr m -> Fn state repr m
make = Fn


run
    :: forall state repr m
    .  MonadRec m => MonadEffect m
    => HasFallback repr
    => Raw.Protocol state repr
    -> Fn state repr m
    -> m ( state /\ Map InletR repr /\ Map OutletR repr )
run protocol (Fn _ process) = do
    _ <- RawProcess.runM protocol process
    nextState <- liftEffect $ RawProtocol.getState protocol
    nextInlets <- liftEffect $ RawProtocol.getInlets protocol
    nextOutlets <- liftEffect $ RawProtocol.getOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


run' :: forall state repr m. MonadRec m => MonadEffect m => HasFallback repr => Raw.Protocol state repr -> Fn state repr m -> m Unit
run' protocol (Fn _ process) =
    RawProcess.runM protocol process