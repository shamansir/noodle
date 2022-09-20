module Noodle.Fn.Process where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Effect (Effect)
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
import Record as Record
import Prim.Row (class Cons)

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

data ProcessF state is os d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' (forall o. Output o) d a -- TODO: use strings here, but close the constructors?
    -- | SendIn (forall proxy i. IsSymbol i => proxy i) d a
    -- | SendIn (forall i. Input i) d a
    -- | SendIn (forall i. SProxy i) d a
    | SendIn (forall i. Input i) d a -- TODO: use strings here, but close the constructors?
    | Receive' (forall i. Input i) (d -> a) -- TODO: use strings here, but close the constructors?
    -- Connect
    -- Disconnect etc



instance functorProcessF :: Functor m => Functor (ProcessF state is os d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next


newtype ProcessM state is os d m a = ProcessM (Free (ProcessF state is os d m) a)


derive newtype instance functorProcessM :: Functor (ProcessM state is os d m)
derive newtype instance applyProcessM :: Apply (ProcessM state is os d m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state is os d m)
derive newtype instance bindProcessM :: Bind (ProcessM state is os d m)
derive newtype instance monadProcessM :: Monad (ProcessM state is os d m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state is os d m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state is os d m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state is os d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (ProcessM state is os d m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM state is os d m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM state is os d m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM state is os d m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecHalogenM :: MonadRec (ProcessM state is os d m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall i state is os d m. IsSymbol i => Input i -> ProcessM state is os d m d
receive iid = ProcessM $ Free.liftF $ Receive' (unsafeCoerce iid) identity


send :: forall o state is os d m. IsSymbol o => Output o -> d -> ProcessM state is os d m Unit
send oid d = ProcessM $ Free.liftF $ Send' (unsafeCoerce oid) d unit


-- sendIn :: forall i state d m. Input i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ proxy i o state is os d m. IsSymbol i => Input i → d → ProcessM state is os d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


sendIn' ∷ ∀ proxy i o state is os d m. IsSymbol i => Input i → d → ProcessM state is os d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn' iid d =
    ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


testSendIn ∷ ∀ state is os d m. d → ProcessM state is os d m Unit
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

lift :: forall i o state is os d m. m Unit -> ProcessM state is os d m Unit
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


imapFState :: forall state state' is os d m. (state -> state') -> (state' -> state) -> ProcessF state is os d m ~> ProcessF state' is os d m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


imapMState :: forall state state' is os d m. (state -> state') -> (state' -> state) -> ProcessM state is os d m ~> ProcessM state' is os d m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


imapFFocus :: forall i o state is os d d' m. (d -> d') -> (d' -> d) -> ProcessF state is os d m ~> ProcessF state is os d' m
imapFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next
        SendIn iid d next -> SendIn iid (f d) next


imapMFocus :: forall state is os d d' m. (d -> d') -> (d' -> d) -> ProcessM state is os d m ~> ProcessM state is os d' m
imapMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapFM :: forall state is os d m m'. (m ~> m') -> ProcessF state is os d m ~> ProcessF state is os d m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


mapMM :: forall state is os d m m'. (m ~> m') -> ProcessM state is os d m ~> ProcessM state is os d m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


{-
runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree -}




type TestInputs = ( foo :: String, i2 :: Boolean )
type TestOutputs = ( bar :: Int, o2 :: Boolean )


runM :: forall is os state d m. MonadEffect m => MonadRec m => Ref (Record is)
    -> Ref (Record os)
    -> d
    -> Ref state
    -> ProcessM state is os d m
    ~> m
runM inputsRef outputsRef default stateRef (ProcessM processFree) =
    runFreeM inputsRef outputsRef default stateRef processFree


-- TODO: pass the inputs / outputs records here, with the current content and so the scheme for types, they can be stored in `Protocol`.
-- runFreeM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> Free (ProcessF state d m) ~> m
-- runFreeM :: forall is os state d m. MonadEffect m => MonadRec m => Row is -> Row os -> d -> Ref state -> Free (ProcessF state d m) ~> m
runFreeM
    :: forall is os state d m
     . MonadEffect m
    => MonadRec m
    => Ref (Record is)
    -> Ref (Record os)
    -> d
    -> Ref state
    -> Free (ProcessF state is os d m)
    ~> m
runFreeM inputsRef outputsRef default stateRef fn =
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
            valueAtInput <- getInputAt iid
            pure
                $ getV
                $ valueAtInput
        go (Send' oid v next) = do
            sendToOutput oid v
            pure next
        go (SendIn iid v next) = do
            sendToInput iid v
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef
        getInputAt :: forall i. Cons i d is is => IsSymbol i => Input i -> m d
        getInputAt iid = liftEffect $ Record.get iid <$> Ref.read inputsRef
        sendToOutput :: forall o. Cons o d os os => IsSymbol o => Output o -> d -> m Unit
        sendToOutput oid v = liftEffect $ Ref.modify_ (Record.set oid v) outputsRef
        sendToInput :: forall i. Cons i d is is => IsSymbol i => Input i -> d -> m Unit
        sendToInput iid v = liftEffect $ Ref.modify_ (Record.set iid v) inputsRef