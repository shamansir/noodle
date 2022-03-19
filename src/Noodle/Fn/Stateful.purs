module Noodle.Fn.Stateful
    ( Fn, Fn', Name, make, make'
    , InputId(..), OutputId(..)
    , run, run'
    , with, with'
    , name
    , shapeOf, dimensions, dimensionsBy, dimensionsBy'
    , findInput, findOutput
    , mapInputs, mapOutputs, mapInputsAndOutputs
    , mapInputsIds, mapOutputsIds, mapInputsAndOutputsIds
    , in_, out_, _in, _out
    )
    where


import Prelude

import Data.Newtype (class Newtype, unwrap)

import Data.Array as Array
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)

import Control.Monad.Rec.Class (class MonadRec)

import Noodle.Fn.Process (ProcessM)
import Noodle.Fn.Process as Process
import Noodle.Fn.Transfer (Receive, Send)
import Noodle.Fn.Transfer as T


type Name = String


{-
    - `i` -> input ID
    - `ii` -> input info (i.e. channel)
    - `o` -> output ID
    - `oo` -> output info (i.e. channel)
    - `state` -> function state (to be shared / reused)
    - `m` -> monad where the function will be run
    - `d` -> data pass through the inputs/outputs
-}
data Fn' i ii o oo state m d = Fn Name (Array (i /\ ii)) (Array (o /\ oo)) (ProcessM i o state d m Unit)


type Fn i o state m d = Fn' i Unit o Unit state m d


instance invariantFn :: Invariant (Fn' i ii o oo state m) where
    imap :: forall i ii o oo state m d d'. (d -> d') -> (d' -> d) -> Fn' i ii o oo state m d -> Fn' i ii o oo state m d'
    imap f g (Fn name is os processM) = Fn name is os $ Process.imapMFocus f g processM


newtype InputId = InputId String

derive newtype instance eqInputId :: Eq InputId
derive newtype instance ordInputId :: Ord InputId
derive newtype instance showInputId :: Show InputId
derive instance newtypeInputId :: Newtype InputId _

newtype OutputId = OutputId String

derive newtype instance eqOutputId :: Eq OutputId
derive newtype instance ordOutputId :: Ord OutputId
derive newtype instance showOutputId :: Show OutputId
derive instance newtypeOutputId :: Newtype OutputId _


-- type Process i o d m = Receive i d -> m (Pass o d)


in_ :: String -> InputId
in_ = InputId


_in :: InputId -> String
_in = unwrap


out_ :: String -> OutputId
out_ = OutputId


_out :: OutputId -> String
_out = unwrap


{- Creating -}


make :: forall i o state m d. Name -> Array i -> Array o -> ProcessM i o state d m Unit -> Fn i o state m d
make name inputs outputs = make' name ((\i -> i /\ unit) <$> inputs) ((\o -> o /\ unit) <$> outputs)


make' :: forall i ii o oo state m d. Name -> Array (i /\ ii) -> Array (o /\ oo) -> ProcessM i o state d m Unit -> Fn' i ii o oo state m d
make' = Fn


{-
program :: forall state d m. MonadEffect m => ProcessM state d m Unit
program = do
    x <- receive $ in_ "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    -- pure (x + n)
    pure unit

-}

mapInputs :: forall i ii ii' o oo state m d. (ii -> ii') -> Fn' i ii o oo state m d -> Fn' i ii' o oo state m d
mapInputs f (Fn name is os processM) = Fn name (rmap f <$> is) os processM


mapInputsIds :: forall i i' ii o oo state m d. (i -> i') -> Fn' i ii o oo state m d -> Fn' i' ii o oo state m d
mapInputsIds f (Fn name is os processM) = Fn name (lmap f <$> is) os $ Process.mapMInputs f processM


mapOutputs :: forall i ii o oo oo' state m d. (oo -> oo') -> Fn' i ii o oo state m d -> Fn' i ii o oo' state m d
mapOutputs f (Fn name is os processM) = Fn name is (rmap f <$> os) processM


mapOutputsIds :: forall i ii o o' oo state m d. (o -> o') -> Fn' i ii o oo state m d -> Fn' i ii o' oo state m d
mapOutputsIds f (Fn name is os processM) = Fn name is (lmap f <$> os) $ Process.mapMOutputs f processM


mapInputsAndOutputs :: forall i ii ii' o oo oo' state m d. (ii -> ii') -> (oo -> oo') -> Fn' i ii o oo state m d -> Fn' i ii' o oo' state m d
mapInputsAndOutputs f g = mapInputs f >>> mapOutputs g


mapInputsAndOutputsIds :: forall i i' ii o o' oo state m d. (i -> i') -> (o -> o') -> Fn' i ii o oo state m d -> Fn' i' ii o' oo state m d
mapInputsAndOutputsIds f g = mapInputsIds f >>> mapOutputsIds g


{- Running -}


run :: forall i ii o oo state d m. MonadRec m => MonadEffect m => Ord i => d -> state -> Send o d -> Receive i d -> Fn' i ii o oo state m d -> m Unit
run default state send receive (Fn _ _ _ processM) =
    Process.runM receive send default state processM


run' :: forall i ii o oo state d m. MonadRec m => MonadEffect m => Ord i => d -> state -> (o -> d -> Effect Unit) -> Fn' i ii o oo state m d -> m Unit
run' default state send =
    run default state (T.s send) T.r_


{- mkRun :: forall i ii o oo m d. Name -> d -> Array (i /\ d) -> Array o -> (o -> d -> Effect Unit) -> ProcessM i o Unit d m Unit -> Aff Unit
mkRun name default inlets outlets send processM =
    Process.runM (T.r inlets) send default unit processM


mkRun' :: forall i ii o oo d. Name -> d -> Array (i /\ ii /\ d) -> Array (o /\ oo) -> (o -> d -> Effect Unit) -> ProcessM i o Unit d m Unit -> Aff Unit
mkRun' name default inlets outlets send processM =
    Process.runM receive send default state processM


mkRun'' :: forall i o state d. Name -> d -> state -> Array (i /\ d) -> Array o -> (o -> d -> Effect Unit) -> ProcessM i o state d m Unit -> Aff Unit
mkRun'' name default inlets outlets send processM =
    Process.runM receive send default state processM


mkRun''' :: forall io oo state d. Name -> d -> state -> Array (i /\ ii /\ d) -> Array (o /\ oo) -> (o -> d -> Effect Unit) -> ProcessM i o state d m Unit -> Aff Unit
mkRun''' name default inlets outlets send processM =
    Process.runM receive send default state processM -}


-- run :: forall state d m a. state -> Network d -> NoodleM state d m a -> Effect (state /\ Network d)
-- run state nw = case _ of
--     _ -> pure $ state /\ nw


{- Get information about the function -}


name :: forall i ii o oo state m d. Fn' i ii o oo state m d -> Name
name (Fn n _ _ _) = n


shapeOf :: forall i ii o oo state m d. Fn' i ii o oo state m d -> Array (i /\ ii) /\ Array (o /\ oo)
shapeOf (Fn _ inputs outputs _) = inputs /\ outputs


dimensions :: forall i ii o oo state m d. Fn' i ii o oo state m d -> Int /\ Int
dimensions = shapeOf >>> bimap Array.length Array.length


dimensionsBy :: forall i ii o oo state m d. (i -> Boolean) -> (o -> Boolean) -> Fn' i ii o oo state m d -> Int /\ Int
dimensionsBy iPred oPred = shapeOf >>> bimap (Array.filter (Tuple.fst >>> iPred) >>> Array.length) (Array.filter (Tuple.fst >>> oPred) >>> Array.length)


dimensionsBy' :: forall i ii o oo state m d. (ii -> Boolean) -> (oo -> Boolean) -> Fn' i ii o oo state m d -> Int /\ Int
dimensionsBy' iPred oPred = shapeOf >>> bimap (Array.filter (Tuple.snd >>> iPred) >>> Array.length) (Array.filter (Tuple.snd >>> oPred) >>> Array.length)


findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn' i ii o oo state m d -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn' i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs


changeProcess :: forall i ii o oo state m d. Fn' i ii o oo state m d -> ProcessM i o state d m Unit -> Fn' i ii o oo state m d
changeProcess (Fn name inputs outputs _) newProcessM =
    Fn name inputs outputs newProcessM


with :: forall i ii o oo state m d. Ord i => MonadRec m => MonadEffect m => Fn' i ii o oo state m d -> d -> state -> Send o d -> Receive i d -> ProcessM i o state d m Unit -> m Unit
with fn def state send receive =
    changeProcess fn >>> run def state send receive


with' :: forall i ii o oo state m d. Ord i => MonadRec m => MonadEffect m => Fn' i ii o oo state m d -> d -> state -> (o -> d -> Effect Unit) -> ProcessM i o state d m Unit -> m Unit
with' fn def state send =
    changeProcess fn >>> run' def state send