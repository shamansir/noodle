module Noodle.Fn.Process where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State.Class (class MonadState)

import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Noodle.Fn.Transfer as T


data ProcessF i o state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' o d a
    | Receive' i (d -> a)
    -- Connect
    -- Disconnect etc.


instance functorProcessF :: Functor m => Functor (ProcessF i o state d m) where
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
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM i o state d m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM i o state d m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM i o state d m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


  {- Processing -}

receive :: forall i o state d m. i -> ProcessM i o state d m d
receive iid = ProcessM $ Free.liftF $ Receive' iid identity


send :: forall i o state d m. o -> d -> ProcessM i o state d m Unit
send oid d = ProcessM $ Free.liftF $ Send' oid d unit


{- Maps -}


mapFInputs :: forall i i' o state d m. (i -> i') -> ProcessF i o state d m ~> ProcessF i' o state d m
mapFInputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' (f iid) k
        Send' oid d next -> Send' oid d next


mapFOutputs :: forall i o o' state d m. (o -> o') -> ProcessF i o state d m ~> ProcessF i o' state d m
mapFOutputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' (f oid) d next


mapMInputs :: forall i i' o state d m. (i -> i') -> ProcessM i o state d m ~> ProcessM i' o state d m
mapMInputs f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFInputs f) processFree


mapMOutputs :: forall i o o' state d m. (o -> o') -> ProcessM i o state d m ~> ProcessM i o' state d m
mapMOutputs f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFOutputs f) processFree


imapFFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessF i o state d m ~> ProcessF i o state d' m
imapFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next


imapMFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessM i o state d m ~> ProcessM i o state d' m
imapMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


runM :: forall i o state d. Ord i => T.Receive i d -> T.Send o d -> d -> state -> ProcessM i o state d Aff ~> Aff
runM receive send default state (ProcessM processFree) = do
    stateRef <- liftEffect $ Ref.new state
    runFreeM receive send default stateRef processFree


runFreeM :: forall i o state d. Ord i => T.Receive i d -> T.Send o d -> d -> Ref state -> Free (ProcessF i o state d Aff) ~> Aff
runFreeM (T.Receive { last, fromInputs }) (T.Send sendFn) default stateRef =
    --foldFree go-- (go stateRef)
    Free.runFreeM go
    where
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState nextState
                    pure next
        go (Lift m) = m
        go (Receive' iid getV) =
            pure
                $ getV
                $ Maybe.fromMaybe default
                $ Map.lookup iid fromInputs
        go (Send' outlet v next) = do
            liftEffect $ sendFn outlet v
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef