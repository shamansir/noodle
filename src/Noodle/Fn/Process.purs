module Noodle.Fn.Process where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))

import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Noodle.Fn.Protocol (Protocol)


data ProcessF i o state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' o d a
    | SendIn i d a
    | Receive' i (d -> a)
    -- Connect
    -- Disconnect etc.


instance functorProcessF :: Functor m => Functor (ProcessF i o state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next


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


instance monadRecHalogenM :: MonadRec (ProcessM i o state d m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall i o state d m. i -> ProcessM i o state d m d
receive iid = ProcessM $ Free.liftF $ Receive' iid identity


send :: forall i o state d m. o -> d -> ProcessM i o state d m Unit
send oid d = ProcessM $ Free.liftF $ Send' oid d unit


sendIn :: forall i o state d m. i -> d -> ProcessM i o state d m Unit
sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit


lift :: forall i o state d m. m Unit -> ProcessM i o state d m Unit
lift m = ProcessM $ Free.liftF $ Lift m


-- lift :: forall i o state d m a. m a -> ProcessM i o state d m a
-- lift m = ProcessM $ Free.liftF $ Lift m


{- Maps -}


mapFInputs :: forall i i' o state d m. (i -> i') -> ProcessF i o state d m ~> ProcessF i' o state d m
mapFInputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' (f iid) k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn (f iid) d next


mapFOutputs :: forall i o o' state d m. (o -> o') -> ProcessF i o state d m ~> ProcessF i o' state d m
mapFOutputs f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' (f oid) d next
        SendIn iid d next -> SendIn iid d next


mapMInputs :: forall i i' o state d m. (i -> i') -> ProcessM i o state d m ~> ProcessM i' o state d m
mapMInputs f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFInputs f) processFree


mapMOutputs :: forall i o o' state d m. (o -> o') -> ProcessM i o state d m ~> ProcessM i o' state d m
mapMOutputs f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFOutputs f) processFree


imapFState :: forall i o state state' d m. (state -> state') -> (state' -> state) -> ProcessF i o state d m ~> ProcessF i o state' d m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


imapMState :: forall i o state state' d m. (state -> state') -> (state' -> state) -> ProcessM i o state d m ~> ProcessM i o state' d m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


imapFFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessF i o state d m ~> ProcessF i o state d' m
imapFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next
        SendIn iid d next -> SendIn iid (f d) next


imapMFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessM i o state d m ~> ProcessM i o state d' m
imapMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapFM :: forall i o state d m m'. (m ~> m') -> ProcessF i o state d m ~> ProcessF i o state d m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


mapMM :: forall i o state d m m'. (m ~> m') -> ProcessM i o state d m ~> ProcessM i o state d m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree



runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> state -> ProcessM i o state d m ~> m
runM protocol default state (ProcessM processFree) = do
    stateRef <- liftEffect $ Ref.new state
    runFreeM protocol default stateRef processFree


runFreeM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> Free (ProcessF i o state d m) ~> m
runFreeM protocol default stateRef fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState nextState
                    pure next
        go (Lift m) = m
        go (Receive' iid getV) = do
            maybeVal <- liftEffect $ protocol.receive iid
            pure
                $ getV
                $ Maybe.fromMaybe default -- FIXME: should either be Maybe or default of particular input channel
                $ maybeVal
        go (Send' output v next) = do
            liftEffect $ protocol.send output v
            pure next
        go (SendIn input v next) = do
            liftEffect $ protocol.sendIn input v
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef