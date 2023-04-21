module Noodle.Fn2
  ( Fn
  , class ToFn, toFn
  , Name, name
  , Orders
  , make, run, run'
  , shape
  --, with
--   , _in, in_, _out, out_
  , dimensions, dimensionsBy --, dimensionsBy'
  -- , findInput, findOutput
  , mapM
  , imapState
  , cloneReplace
  , inputsShape, outputsShape
  , inputsOrder, outputsOrder
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
import Data.SOrder (SOrder, class HasSymbolsOrder)
import Data.SOrder (instantiate) as SOrder

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Id (class HasInputsAt, class HasOutputsAt, InputR, OutputR, fromKeysR, HoldsInput, HoldsOutput)
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as Process
import Noodle.Fn2.Protocol (Protocol)
import Noodle.Fn2.Protocol as Protocol


type Name = String


type Orders (iorder :: SOrder) (oorder :: SOrder) =
    { inputs :: Proxy iorder, outputs :: Proxy oorder }


data Fn state (is :: Row Type) (os :: Row Type) m = Fn Name { inputs :: SOrder, outputs :: SOrder } (ProcessM state is os m Unit)


class ToFn a state is os where
    toFn :: forall m. a -> Fn state is os m


make :: forall state is iorder os oorder m. HasSymbolsOrder iorder is => HasSymbolsOrder oorder os => Name -> Orders iorder oorder -> ProcessM state is os m Unit -> Fn state is os m
make name order = Fn name { inputs : SOrder.instantiate (Proxy :: _ is) order.inputs, outputs : SOrder.instantiate (Proxy :: _ os) order.outputs }


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


mapM :: forall state is os m m'. (m ~> m') -> Fn state is os m -> Fn state is os m'
mapM f (Fn name order processM) = Fn name order $ Process.mapMM f processM


imapState :: forall state state' is os m. (state -> state') -> (state' -> state) -> Fn state is os m -> Fn state' is os m
imapState f g (Fn name order processM) = Fn name order $ Process.imapMState f g processM

{- Running -}

{-
run :: forall i ii o oo state d m. MonadRec m => MonadEffect m => Ord i => d -> state -> Protocol i o d -> Fn i ii o oo state m d -> m state
run default state protocol (Fn _ _ _ processM) = do
    stateRef :: Ref state <- liftEffect $ Ref.new state
    Process.runM protocol default stateRef processM
    liftEffect $ Ref.read stateRef
-}


run :: forall state is os m. MonadRec m => MonadEffect m => Protocol state is os -> Fn state is os m -> m ( state /\ Record is /\ Record os )
run protocol (Fn _ _ process) = do
    _ <- Process.runM protocol process
    nextState <- liftEffect $ protocol.getState unit
    nextInputs <- liftEffect $ Tuple.snd <$> protocol.getInputs unit
    nextOutputs <- liftEffect $ Tuple.snd <$> protocol.getOutputs unit
    pure $ nextState /\ nextInputs /\ nextOutputs


run' :: forall state is os m. MonadRec m => MonadEffect m => Protocol state is os -> Fn state is os m -> m Unit
run' protocol (Fn _ _ process) =
    Process.runM protocol process


{- Get information about the function -}


name :: forall state is os m. Fn state is os m -> Name
name (Fn n _ _) = n


inputsShape :: forall state (is :: Row Type) os m rli. HasInputsAt is rli => Fn state is os m -> List InputR
inputsShape (Fn _ { inputs } _) = fromKeysR inputs (Proxy :: Proxy is)


outputsShape :: forall state is (os :: Row Type) m rlo. HasOutputsAt os rlo => Fn state is os m -> List OutputR
outputsShape (Fn _ { outputs } _) = fromKeysR outputs (Proxy :: Proxy os)


{- inputsShapeH :: forall state (is :: Row Type) os m rli. HasInputsAt is rli => Fn state is os m -> List HoldsInput
inputsShapeH (Fn _ { inputs } _) = fromKeysR inputs (Proxy :: Proxy is)


outputsShapeH :: forall state is (os :: Row Type) m rlo. HasOutputsAt os rlo => Fn state is os m -> List HoldsOutput
outputsShapeH (Fn _ { outputs } _) = fromKeysR outputs (Proxy :: Proxy os) -}


inputsOrder :: forall state (is :: Row Type) os m rli. HasInputsAt is rli => Fn state is os m -> SOrder
inputsOrder (Fn _ { inputs } _) = inputs


outputsOrder :: forall state (is :: Row Type) os m rlo. HasOutputsAt os rlo => Fn state is os m -> SOrder
outputsOrder (Fn _ { outputs } _) = outputs



-- TODO: mapRecord


shape
    :: forall state (is :: Row Type) (os :: Row Type) m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => Fn state is os m
    -> List InputR /\ List OutputR
shape fn = inputsShape fn /\ outputsShape fn


dimensions
    :: forall state is os m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => Fn state is os m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall state is os m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => (InputR -> Boolean)
    -> (OutputR -> Boolean)
    -> Fn state is os m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)


{-
findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os m -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}


cloneReplace :: forall state is os m. Fn state is os m -> ProcessM state is os m Unit -> Fn state is os m
cloneReplace (Fn name order _) newProcessM =
    Fn name order newProcessM


{-}
with :: forall i ii o oo state m d. Ord i => MonadRec m => MonadState state m => MonadEffect m => Fn i ii o oo state m d -> d -> state -> Protocol i o d -> ProcessM i o state d m Unit -> m state
with fn def state protocol =
    changeProcess fn >>> run def state protocol
    -}