module Noodle.Fn.Process where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Noodle.Fn.Protocol (Protocol)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- type Input (s :: Symbol) = SProxy
-- type Output (s :: Symbol) = SProxy


data Input (s :: Symbol) = Input
data Output (s :: Symbol) = Output


-- testInput :: forall proxy s. IsSymbol s => proxy s -> Input s
_testInputP = Proxy :: Proxy "foo"
_testInput = Input :: Input "foo"


{-
data ProcessF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' (forall o. IsSymbol o => Output o) d a
    -- | SendIn (forall proxy i. IsSymbol i => proxy i) d a
    -- | SendIn (forall i. Input i) d a
    -- | SendIn (forall i. SProxy i) d a
    | SendIn (forall i. IsSymbol i => Input i) d a
    | Receive' (forall i. IsSymbol i => Input i) (d -> a)
    -- Connect
    -- Disconnect etc.
-}

data ProcessF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' (forall o. Output o) d a
    -- | SendIn (forall proxy i. IsSymbol i => proxy i) d a
    -- | SendIn (forall i. Input i) d a
    -- | SendIn (forall i. SProxy i) d a
    | SendIn (forall i. Input i) d a
    | Receive' (forall i. Input i) (d -> a)
    -- Connect
    -- Disconnect etc



instance functorProcessF :: Functor m => Functor (ProcessF state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next


newtype ProcessM state d m a = ProcessM (Free (ProcessF state d m) a)


derive newtype instance functorProcessM :: Functor (ProcessM state d m)
derive newtype instance applyProcessM :: Apply (ProcessM state d m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state d m)
derive newtype instance bindProcessM :: Bind (ProcessM state d m)
derive newtype instance monadProcessM :: Monad (ProcessM state d m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state d m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state d m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM i o state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (ProcessM state d m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM state d m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM state d m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM state d m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecHalogenM :: MonadRec (ProcessM state d m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall i state d m. IsSymbol i => Input i -> ProcessM state d m d
receive iid = ProcessM $ Free.liftF $ Receive' (unsafeCoerce iid) identity


send :: forall o state d m. IsSymbol o => Output o -> d -> ProcessM state d m Unit
send oid d = ProcessM $ Free.liftF $ Send' (unsafeCoerce oid) d unit


-- sendIn :: forall i state d m. Input i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ proxy i o state d m. IsSymbol i => Input i → d → ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    let test = reflectSymbol iid :: String
    in ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


sendIn' ∷ ∀ proxy i o state d m. IsSymbol i => Input i → d → ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn' iid d =
    ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


testSendIn ∷ ∀ state d m. d → ProcessM state d m Unit
testSendIn = sendIn _testInput


{-
testExtract ::  ∀ state d m proxy s. IsSymbol s => ProcessF state d m Unit -> Maybe String
testExtract =
    case _ of
        State k -> Nothing
        Lift m -> Nothing
        Receive' iid k -> Just $ reflectSymbol (iid :: Input s)
        Send' oid d next -> Just $ reflectSymbol (oid :: Output s)
        SendIn iid d next -> Just $ reflectSymbol (iid :: Input s)
-}

lift :: forall i o state d m. m Unit -> ProcessM state d m Unit
lift m = ProcessM $ Free.liftF $ Lift m


-- lift :: forall i o state d m a. m a -> ProcessM i o state d m a
-- lift m = ProcessM $ Free.liftF $ Lift m


{- Maps -}


{-
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
-}


imapFState :: forall i o state state' d m. (state -> state') -> (state' -> state) -> ProcessF state d m ~> ProcessF state' d m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


imapMState :: forall i o state state' d m. (state -> state') -> (state' -> state) -> ProcessM state d m ~> ProcessM state' d m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


imapFFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessF state d m ~> ProcessF state d' m
imapFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next
        SendIn iid d next -> SendIn iid (f d) next


imapMFocus :: forall i o state d d' m. (d -> d') -> (d' -> d) -> ProcessM state d m ~> ProcessM state d' m
imapMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapFM :: forall i o state d m m'. (m ~> m') -> ProcessF state d m ~> ProcessF state d m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


mapMM :: forall i o state d m m'. (m ~> m') -> ProcessM state d m ~> ProcessM state d m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree


runFreeM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> Free (ProcessF state d m) ~> m
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