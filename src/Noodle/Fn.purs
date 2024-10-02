module Noodle.Fn
  ( Fn
  , class ToFn, toFn
  , name
  , make, run, run', runRec
  , mapM
  , imapState
  , cloneReplace
  , toRaw, toRawWithReprableState
  )
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
import Data.Map (Map)
-- import Data.SOrder (SOrder, class HasSymbolsOrder)
-- import Data.SOrder (instantiate) as SOrder

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Id (FnName, Inlet, Outlet, InletR, OutletR)
-- import Noodle.Node.Has (class HasInletsAt, class HasOutletsAt)
import Noodle.Fn.Process (Process)
import Noodle.Fn.Process as Process
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Raw.Fn (Fn(..)) as Raw
import Noodle.Repr (class HasFallback, class FromReprRow, class FromRepr, class ToRepr)


data Fn state (is :: Row Type) (os :: Row Type) repr (m :: Type -> Type) = Fn FnName (Process state is os repr m)


toRaw :: forall state is os repr m. Fn state is os repr m -> Raw.Fn state repr m
toRaw (Fn name processM) = Raw.Fn name $ Process.toRaw processM


toRawWithReprableState :: forall state is os repr m. FromRepr repr state => ToRepr state repr => Fn state is os repr m -> Raw.Fn repr repr m
toRawWithReprableState (Fn name processM) = Raw.Fn name $ Process.toRawWithReprableState processM


class ToFn a state is os repr where
    toFn :: forall m. a -> Fn state is os repr m


make :: forall state is os repr m. FnName -> Process state is os repr m -> Fn state is os repr m
make = Fn


{- Creating -}


mapM :: forall state is os repr m m'. (m ~> m') -> Fn state is os repr m -> Fn state is os repr m'
mapM f (Fn name processM) = Fn name $ Process.mapMM f processM


imapState :: forall state state' is os repr m. (state -> state') -> (state' -> state) -> Fn state is os repr m -> Fn state' is os repr m
imapState f g (Fn name processM) = Fn name $ Process.imapMState f g processM

{- Running -}

run
    :: forall state is os repr m
    .  MonadRec m => MonadEffect m
    => HasFallback repr
    => Protocol state is os repr
    -> Fn state is os repr m
    -> m ( state /\ Map InletR repr /\ Map OutletR repr )
run protocol (Fn _ process) = do
    _ <- Process.runM protocol process
    nextState <- liftEffect $ Protocol.getState protocol
    nextInlets <- liftEffect $ Protocol.getInlets protocol
    nextOutlets <- liftEffect $ Protocol.getOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


runRec
    :: forall state is os isrl osrl repr m
    .  MonadRec m => MonadEffect m
    => HasFallback repr
    => RL.RowToList is isrl => FromReprRow isrl is repr
    => RL.RowToList os osrl => FromReprRow osrl os repr
    => Protocol state is os repr
    -> Fn state is os repr m
    -> m ( state /\ Record is /\ Record os )
runRec protocol (Fn _ process) = do
    _ <- Process.runM protocol process
    nextState <- liftEffect $ Protocol.getState protocol
    nextInlets <- liftEffect $ Protocol.getRecInlets protocol
    nextOutlets <- liftEffect $ Protocol.getRecOutlets protocol
    pure $ nextState /\ nextInlets /\ nextOutlets


run' :: forall state is os repr m. MonadRec m => MonadEffect m => HasFallback repr => Protocol state is os repr -> Fn state is os repr m -> m Unit
run' protocol (Fn _ process) =
    Process.runM protocol process


{- Get information about the function -}



name :: forall state is os repr m. Fn state is os repr m -> FnName
name (Fn n _) = n


cloneReplace :: forall state is os repr m. Fn state is os repr m -> Process state is os repr m -> Fn state is os repr m
cloneReplace (Fn name _) newProcessM =
    Fn name newProcessM