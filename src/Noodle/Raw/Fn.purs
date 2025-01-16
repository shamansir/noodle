module Noodle.Raw.Fn where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Map (Map)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (FnName, InletR, OutletR)

import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (runM, toReprableState) as RawProcess
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Raw.Fn.Protocol (getState, getInlets, getOutlets) as RawProtocol
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)


data Fn state chrepr (m :: Type -> Type) = Fn FnName (Raw.Process state chrepr m) -- TODO: move to separate module


make :: forall state chrepr m. FnName -> Raw.Process state chrepr m -> Fn state chrepr m
make = Fn


run
    :: forall state chrepr m
    .  MonadRec m => MonadEffect m
    => HasFallback chrepr
    => Raw.Protocol state chrepr
    -> Fn state chrepr m
    -> m ( state /\ Map InletR (ValueInChannel chrepr) /\ Map OutletR (ValueInChannel chrepr) )
run protocol (Fn _ process) = do
    _ <- RawProcess.runM protocol process
    nextState <- liftEffect $ RawProtocol.getState protocol
    nextInlets <- liftEffect $ RawProtocol.getInlets protocol
    nextOutlets <- liftEffect $ RawProtocol.getOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


run' :: forall state chrepr m. MonadRec m => MonadEffect m => HasFallback chrepr => Raw.Protocol state chrepr -> Fn state chrepr m -> m Unit
run' protocol (Fn _ process) =
    RawProcess.runM protocol process


toReprableState :: forall state strepr chrepr m. HasFallback state => StRepr state strepr => Fn state chrepr m -> Fn strepr chrepr m
toReprableState (Fn name processFn) = Fn name $ RawProcess.toReprableState processFn