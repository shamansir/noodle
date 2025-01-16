module Noodle.Fn
  ( Fn
  , class ToFn, toFn
  , name
  , make, run, run', runRec
  , mapM
  , imapState
  , cloneReplace
  , toReprableState
  , toRaw, toRawWithReprableState
  )
  where


import Prelude

import Prim.RowList as RL

import Data.Tuple.Nested (type (/\), (/\))
import Data.Map (Map)
-- import Data.SOrder (SOrder, class HasSymbolsOrder)
-- import Data.SOrder (instantiate) as SOrder

import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.Rec.Class (class MonadRec)

import Noodle.Id (FnName, InletR, OutletR)
-- import Noodle.Node.Has (class HasInletsAt, class HasOutletsAt)
import Noodle.Fn.Process (Process)
import Noodle.Fn.Process as Process
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Raw.Fn (Fn(..)) as Raw
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (ensureFrom, to) as StRepr
import Noodle.Repr.ValueInChannel (ValueInChannel, class ToValuesInChannelRow)
-- import Noodle.Repr.ChRepr (ensureTo, ensureFrom, wrap, unwrap) as ChRepr


data Fn state (is :: Row Type) (os :: Row Type) chrepr (m :: Type -> Type) = Fn FnName (Process state is os chrepr m)


class ToFn a state is os chrepr where
    toFn :: forall m. a -> Fn state is os chrepr m


{- Creating -}


make :: forall state is os chrepr m. FnName -> Process state is os chrepr m -> Fn state is os chrepr m
make = Fn


{- Mapping -}


mapM :: forall state is os chrepr m m'. (m ~> m') -> Fn state is os chrepr m -> Fn state is os chrepr m'
mapM f (Fn name processM) = Fn name $ Process.mapMM f processM


imapState :: forall state state' is os chrepr m. (state -> state') -> (state' -> state) -> Fn state is os chrepr m -> Fn state' is os chrepr m
imapState f g (Fn name processM) = Fn name $ Process.imapMState f g processM


toReprableState :: forall state strepr is os chrepr m. HasFallback state => StRepr state strepr => Fn state is os chrepr m -> Fn strepr is os chrepr m
toReprableState = imapState StRepr.to StRepr.ensureFrom


{- Running -}

run
    :: forall state is os chrepr m
    .  MonadRec m => MonadEffect m
    => HasFallback chrepr
    => Protocol state is os chrepr
    -> Fn state is os chrepr m
    -> m ( state /\ Map InletR (ValueInChannel chrepr) /\ Map OutletR (ValueInChannel chrepr) )
run protocol (Fn _ process) = do
    _ <- Process.runM protocol process
    nextState <- liftEffect $ Protocol.getState protocol
    nextInlets <- liftEffect $ Protocol.getInlets protocol
    nextOutlets <- liftEffect $ Protocol.getOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


runRec
    :: forall state is os isrl osrl chrepr m
    .  MonadRec m => MonadEffect m
    => HasFallback chrepr
    => RL.RowToList is isrl => ToValuesInChannelRow isrl is chrepr
    => RL.RowToList os osrl => ToValuesInChannelRow osrl os chrepr
    => Protocol state is os chrepr
    -> Fn state is os chrepr m
    -> m ( state /\ Record is /\ Record os )
runRec protocol (Fn _ process) = do
    _ <- Process.runM protocol process
    nextState <- liftEffect $ Protocol.getState protocol
    nextInlets <- liftEffect $ Protocol.getRecInlets protocol
    nextOutlets <- liftEffect $ Protocol.getRecOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


run' :: forall state is os chrepr m. MonadRec m => MonadEffect m => HasFallback chrepr => Protocol state is os chrepr -> Fn state is os chrepr m -> m Unit
run' protocol (Fn _ process) =
    Process.runM protocol process


{- Get information about the function -}



name :: forall state is os chrepr m. Fn state is os chrepr m -> FnName
name (Fn n _) = n


cloneReplace :: forall state is os chrepr m. Fn state is os chrepr m -> Process state is os chrepr m -> Fn state is os chrepr m
cloneReplace (Fn name _) newProcessM =
    Fn name newProcessM


{- Convert -}

toRaw :: forall state is os chrepr m. Fn state is os chrepr m -> Raw.Fn state chrepr m
toRaw (Fn name processM) = Raw.Fn name $ Process.toRaw processM


toRawWithReprableState :: forall state strepr is os chrepr m. HasFallback state => StRepr state strepr => Fn state is os chrepr m -> Raw.Fn strepr chrepr m
toRawWithReprableState (Fn name processM) = Raw.Fn name $ Process.toRawWithReprableState processM
