module Noodle.Fn
  ( Fn
  , class ToFn, toFn
  , Name, name
  , make, run, run', runRec
--   , shape
  --, with
--   , _in, in_, _out, out_
--   , dimensions, dimensionsBy --, dimensionsBy'
  -- , findInlet, findOutlet
  , mapM
  , imapState
  , cloneReplace
--   , inputsShape, outputsShape
--   , inputsShapeHeld, outputsShapeHeld
--   , inputsOrder, outputsOrder
  , RawFn, toRaw
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
import Data.Repr (class HasFallback, class FromReprRow)

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Id (Inlet, Outlet, InletR, OutletR)
-- import Noodle.Node.Has (class HasInletsAt, class HasOutletsAt)
import Noodle.Fn.Process (ProcessM)
import Noodle.Fn.Process as Process
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Fn.Raw.Process (RawProcessM)


type Name = String


data Fn state (is :: Row Type) (os :: Row Type) repr (m :: Type -> Type) = Fn Name (ProcessM state is os repr m Unit)


data RawFn state repr (m :: Type -> Type) = RawFn Name (RawProcessM state repr m Unit) -- TODO: move to separate module


toRaw :: forall state is os repr m. Fn state is os repr m -> RawFn state repr m
toRaw (Fn name processM) = RawFn name $ Process.toRaw processM


class ToFn a state is os repr where
    toFn :: forall m. a -> Fn state is os repr m


make :: forall state is os repr m. Name -> ProcessM state is os repr m Unit -> Fn state is os repr m
make = Fn


{- Creating -}


-- Toolkit? does it store current state?


{- make' :: forall i o state m d. Name -> Array i -> Array o -> ProcessM i o state d m Unit -> Fn' i o state m d
make' name inputs outputs = make name ((\i -> i /\ unit) <$> inputs) ((\o -> o /\ unit) <$> outputs) -}


{-
program :: forall state d m. MonadEffect m => ProcessM state d m Unit
program = do
    x <- receive $ in_ "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    -- pure (x + n)
    pure unit

-}


mapM :: forall state is os repr m m'. (m ~> m') -> Fn state is os repr m -> Fn state is os repr m'
mapM f (Fn name processM) = Fn name $ Process.mapMM f processM


imapState :: forall state state' is os repr m. (state -> state') -> (state' -> state) -> Fn state is os repr m -> Fn state' is os repr m
imapState f g (Fn name processM) = Fn name $ Process.imapMState f g processM

{- Running -}

{-
run :: forall i ii o oo state d m. MonadRec m => MonadEffect m => Ord i => d -> state -> Protocol i o d -> Fn i ii o oo state m d -> m state
run default state protocol (Fn _ _ _ processM) = do
    stateRef :: Ref state <- liftEffect $ Ref.new state
    Process.runM protocol default stateRef processM
    liftEffect $ Ref.read stateRef
-}


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



name :: forall state is os repr m. Fn state is os repr m -> Name
name (Fn n _) = n

{-
inputsShape :: forall state (is :: Row Type) os repr m isrl. HasInletsAt is isrl => Fn state is os repr m -> List InletR
inputsShape (Fn _ { inputs } _) = fromKeysR inputs (Proxy :: _ is)


outputsShape :: forall state is (os :: Row Type) repr m osrl. HasOutletsAt os osrl => Fn state is os repr m -> List OutletR
outputsShape (Fn _ { outputs } _) = fromKeysR outputs (Proxy :: _ os)



inputsShapeHeld :: forall state (is :: Row Type) os repr m isrl. KH.KeysO isrl Inlet HoldsInlet => HasInletsAt is isrl => Fn state is os repr m -> Array HoldsInlet
inputsShapeHeld (Fn _ { inputs } _) = KH.orderedKeys' (Proxy :: _ Inlet) inputs (Proxy :: _ is)


outputsShapeHeld :: forall state is (os :: Row Type) repr m osrl. KH.KeysO osrl Outlet HoldsOutlet => HasOutletsAt os osrl => Fn state is os repr m -> Array HoldsOutlet
outputsShapeHeld (Fn _ { outputs } _) = KH.orderedKeys' (Proxy :: _ Outlet) outputs (Proxy :: _ os)


inputsOrder :: forall state (is :: Row Type) os repr m isrl. HasInletsAt is isrl => Fn state is os repr m -> SOrder
inputsOrder (Fn _ { inputs } _) = inputs


outputsOrder :: forall state (is :: Row Type) os repr m osrl. HasOutletsAt os osrl => Fn state is os repr m -> SOrder
outputsOrder (Fn _ { outputs } _) = outputs



-- TODO: mapRecord


shape
    :: forall state (is :: Row Type) (os :: Row Type) repr m isrl osrl
     . HasInletsAt is isrl
    => HasOutletsAt os osrl
    => Fn state is os repr m
    -> List InletR /\ List OutletR
shape fn = inputsShape fn /\ outputsShape fn


dimensions
    :: forall state is os repr m isrl osrl
     . HasInletsAt is isrl
    => HasOutletsAt os osrl
    => Fn state is os repr m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall state is os repr m isrl osrl
     . HasInletsAt is isrl
    => HasOutletsAt os osrl
    => (InletR -> Boolean)
    -> (OutletR -> Boolean)
    -> Fn state is os repr m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)
-}


{-
findInlet :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os repr m -> Maybe (i /\ ii)
findInlet pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutlet :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutlet pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}


cloneReplace :: forall state is os repr m. Fn state is os repr m -> ProcessM state is os repr m Unit -> Fn state is os repr m
cloneReplace (Fn name _) newProcessM =
    Fn name newProcessM


{-}
with :: forall i ii o oo state m d. Ord i => MonadRec m => MonadState state m => MonadEffect m => Fn i ii o oo state m d -> d -> state -> Protocol i o d -> ProcessM i o state d m Unit -> m state
with fn def state protocol =
    changeProcess fn >>> run def state protocol
    -}