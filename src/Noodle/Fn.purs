module Noodle.Fn
    ( Fn, Name, make
    , InputId(..), OutputId(..)
    , Receive(..), Pass(..), Send(..)
    , receive, send
    , ProcessM
    , runFn
    , name
    , shapeOf, dimensions, dimensionsBy
    , findInput, findOutput
    , mapInputs, mapOutputs, mapInputsAndOutputs
    , in_, out_, _in, _out
    )
    where


import Prelude


import Data.Newtype (class Newtype, unwrap)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF, runFreeM, foldFree)
import Control.Monad.State.Class (class MonadState)

import Data.Array as Array
import Data.Bifunctor (class Bifunctor, lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num.Sets (class Nat)
import Data.Vec (Vec)
import Data.Vec as Vec

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref


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
data Fn i ii o oo state m d = Fn Name (Array (i /\ ii)) (Array (o /\ oo)) (ProcessM i o state d m Unit)


instance invariantFn :: Invariant (Fn i ii o oo state m) where
    imap :: forall i ii o oo state m d d'. (d -> d') -> (d' -> d) -> Fn i ii o oo state m d -> Fn i ii o oo state m d'
    imap f g (Fn name is os processM) = Fn name is os $ imapProcessMFocus f g processM


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


newtype Receive i d = Receive { last :: Maybe i, fromInputs :: i /-> d }


instance functorReceive :: Functor (Receive i) where
    map f (Receive { last, fromInputs }) = Receive { last, fromInputs : f <$> fromInputs }


newtype Pass o d = Pass { toOutputs :: o /-> d }


instance functorPass :: Functor (Pass o) where
    map f (Pass { toOutputs }) = Pass { toOutputs : f <$> toOutputs }


newtype Send o d = Send (o -> d -> Effect Unit)


data ProcessF i o state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' o d a
    | Receive' i (d -> a)
    -- Connect
    -- Disconnect etc.


instance functorNodeF :: Functor m => Functor (ProcessF i o state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next


newtype ProcessM i o state d m a = ProcessM (Free (ProcessF i o state d m) a)


derive newtype instance functorProcessM :: Functor (ProcessM i o state d m)
derive newtype instance applyProcessM :: Apply (ProcessM i o state d m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM i o state d m)
derive newtype instance bindProcessM :: Bind (ProcessM i o state d m)
derive newtype instance monadProcessM :: Monad (ProcessM i o state d m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM i o state d m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM i o state d m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM i o state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (ProcessM i o state d m) where
  liftEffect = ProcessM <<< liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM i o state d m) where
  liftAff = ProcessM <<< liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM i o state d m) where
  state = ProcessM <<< liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM i o state d m) where
  throwError = ProcessM <<< liftF <<< Lift <<< throwError


in_ :: String -> InputId
in_ = InputId


_in :: InputId -> String
_in = unwrap


out_ :: String -> OutputId
out_ = OutputId


_out :: OutputId -> String
_out = unwrap


{- Creating -}


make :: forall i ii o oo state m d. Name -> Array (i /\ ii) -> Array (o /\ oo) -> ProcessM i o state d m Unit -> Fn i ii o oo state m d
make = Fn


{-
program :: forall state d m. MonadEffect m => ProcessM state d m Unit
program = do
    x <- receive $ in_ "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    -- pure (x + n)
    pure unit

-}


{- Processing -}

receive :: forall i o state d m. i -> ProcessM i o state d m d
receive iid = ProcessM $ liftF $ Receive' iid identity


send :: forall i o state d m. o -> d -> ProcessM i o state d m Unit
send oid d = ProcessM $ liftF $ Send' oid d unit


{- Maps -}


mapProcessFInputs :: forall i i' o state d m. (i -> i') -> ProcessF i o state d m ~> ProcessF i' o state d m
mapProcessFInputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' (f iid) k
        Send' oid d next -> Send' oid d next


mapProcessFOutputs :: forall i o o' state d m. (o -> o') -> ProcessF i o state d m ~> ProcessF i o' state d m
mapProcessFOutputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' (f oid) d next


mapProcessMInputs :: forall i i' o state d m. (i -> i') -> ProcessM i o state d m ~> ProcessM i' o state d m
mapProcessMInputs f (ProcessM processFree) =
    ProcessM $ foldFree (liftF <<< mapProcessFInputs f) processFree


mapProcessMOutputs :: forall i o o' state d m. (o -> o') -> ProcessM i o state d m ~> ProcessM i o' state d m
mapProcessMOutputs f (ProcessM processFree) =
    ProcessM $ foldFree (liftF <<< mapProcessFOutputs f) processFree


imapProcessFFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessF i o state d m ~> ProcessF i o state d' m
imapProcessFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next


imapProcessMFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessM i o state d m ~> ProcessM i o state d' m
imapProcessMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (liftF <<< imapProcessFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapInputs :: forall i ii ii' o oo state m d. (ii -> ii') -> Fn i ii o oo state m d -> Fn i ii' o oo state m d
mapInputs f (Fn name is os processM) = Fn name (rmap f <$> is) os processM


mapInputsIds :: forall i i' ii o oo state m d. (i -> i') -> Fn i ii o oo state m d -> Fn i' ii o oo state m d
mapInputsIds f (Fn name is os processM) = Fn name (lmap f <$> is) os $ mapProcessMInputs f processM


mapOutputs :: forall i ii o oo oo' state m d. (oo -> oo') -> Fn i ii o oo state m d -> Fn i ii o oo' state m d
mapOutputs f (Fn name is os processM) = Fn name is (rmap f <$> os) processM


mapOutputsIds :: forall i ii o o' oo state m d. (o -> o') -> Fn i ii o oo state m d -> Fn i ii o' oo state m d
mapOutputsIds f (Fn name is os processM) = Fn name is (lmap f <$> os) $ mapProcessMOutputs f processM


mapInputsAndOutputs :: forall i ii ii' o oo oo' state m d. (ii -> ii') -> (oo -> oo') -> Fn i ii o oo state m d -> Fn i ii' o oo' state m d
mapInputsAndOutputs f g = mapInputs f >>> mapOutputs g


mapInputsAndOutputsIds :: forall i i' ii o o' oo state m d. (i -> i') -> (o -> o') -> Fn i ii o oo state m d -> Fn i' ii o' oo state m d
mapInputsAndOutputsIds f g = mapInputsIds f >>> mapOutputsIds g


{- Running -}


runFn :: forall i ii o oo state d. Receive i d -> Send o d -> d -> state -> Fn i ii o oo state Aff d -> Aff Unit
runFn receive send default state (Fn _ _ _ processM) =
    runProcessM receive send default state processM


runProcessM :: forall i o state d. Receive i d -> Send o d -> d -> state -> ProcessM i o state d Aff ~> Aff
runProcessM receive send default state (ProcessM processFree) = do
    stateRef <- liftEffect $ Ref.new state
    runProcessFreeM receive send default stateRef processFree


runProcessFreeM :: forall i o state d. Receive i d -> Send o d -> d -> Ref state -> Free (ProcessF i o state d Aff) ~> Aff
runProcessFreeM (Receive { last, fromInputs }) (Send sendFn) default stateRef =
    --foldFree go-- (go stateRef)
    runFreeM go
    where
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState nextState
                    pure next
        go (Lift m) = m
        go (Receive' _ getV) = pure $ getV default
        go (Send' outlet v next) = do
            liftEffect $ sendFn outlet v
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef


-- run :: forall state d m a. state -> Network d -> NoodleM state d m a -> Effect (state /\ Network d)
-- run state nw = case _ of
--     _ -> pure $ state /\ nw


{- Get information about the function -}


name :: forall i ii o oo state m d. Fn i ii o oo state m d -> Name
name (Fn n _ _ _) = n


shapeOf :: forall i ii o oo state m d. Fn i ii o oo state m d -> Array (i /\ ii) /\ Array (o /\ oo)
shapeOf (Fn _ inputs outputs _) = inputs /\ outputs


dimensions :: forall i ii o oo state m d. Fn i ii o oo state m d -> Int /\ Int
dimensions = shapeOf >>> bimap Array.length Array.length


dimensionsBy :: forall i ii o oo state m d. (i -> Boolean) -> (o -> Boolean) -> Fn i ii o oo state m d -> Int /\ Int
dimensionsBy iPred oPred = shapeOf >>> bimap (Array.filter (Tuple.fst >>> iPred) >>> Array.length) (Array.filter (Tuple.fst >>> oPred) >>> Array.length)


dimensionsBy' :: forall i ii o oo state m d. (ii -> Boolean) -> (oo -> Boolean) -> Fn i ii o oo state m d -> Int /\ Int
dimensionsBy' iPred oPred = shapeOf >>> bimap (Array.filter (Tuple.snd >>> iPred) >>> Array.length) (Array.filter (Tuple.snd >>> oPred) >>> Array.length)


findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn i ii o oo state m d -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs