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

--data ProcessF :: forall is' os'. Symbol -> Symbol -> Type -> Row is' -> Row os' -> Type -> Type -> (Type -> Type) -> Type
data ProcessF i o state is os din dout m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' (Output o) dout a -- TODO: use strings here, but close the constructors?
    -- | SendIn (forall proxy i. IsSymbol i => proxy i) d a
    -- | SendIn (forall i. Input i) d a
    -- | SendIn (forall i. SProxy i) d a
    | SendIn (Input i) din a -- TODO: use strings here, but close the constructors?
    | Receive' (Input i) (din -> a) -- TODO: use strings here, but close the constructors?
    -- Connect
    -- Disconnect etc



instance functorProcessF :: Functor m => Functor (ProcessF i o state is os din dout m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next


newtype ProcessM i o state is os din dout m a = ProcessM (Free (ProcessF i o state is os din dout m) a)


derive newtype instance functorProcessM :: Functor (ProcessM i o state is os din dout m)
derive newtype instance applyProcessM :: Apply (ProcessM i o state is os din dout m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM i o state is os din dout m)
derive newtype instance bindProcessM :: Bind (ProcessM i o state is os din dout m)
derive newtype instance monadProcessM :: Monad (ProcessM i o state is os din dout m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM i o state is os din dout m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM i o state is os din dout m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM i o state is os din dout m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (ProcessM i o state is os din dout m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM i o state is os din dout m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM i o state is os din dout m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM i o state is os din dout m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecHalogenM :: MonadRec (ProcessM i o state is os din dout m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall i o state is os din dout m. IsSymbol i => Input i -> ProcessM i o state is os din dout m din
receive iid = ProcessM $ Free.liftF $ Receive' (unsafeCoerce iid) identity


send :: forall i o state is os din dout m. IsSymbol o => Output o -> dout -> ProcessM i o state is os din dout m Unit
send oid d = ProcessM $ Free.liftF $ Send' (unsafeCoerce oid) d unit


-- sendIn :: forall i state d m. Input i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ i o state is os din dout m. IsSymbol i => Input i → din → ProcessM i o state is os din dout m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


sendIn' ∷ ∀ i o state is os din dout m. IsSymbol i => Input i → din → ProcessM i o state is os din dout m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn' iid d =
    ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit


testSendIn ∷ ∀ o state os din dout m. din → ProcessM "foo" o state TestInputs os din dout m Unit
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

lift :: forall i o state is os din dout m. m Unit -> ProcessM i o state is os din dout m Unit
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


imapFState :: forall i o state state' is os din dout m. (state -> state') -> (state' -> state) -> ProcessF i o state is os din dout m ~> ProcessF i o state' is os din dout m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


imapMState :: forall i o state state' is os din dout m. (state -> state') -> (state' -> state) -> ProcessM i o state is os din dout m ~> ProcessM i o state' is os din dout m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


imapFInFocus :: forall i o state is os din din' dout m. (din -> din') -> (din' -> din) -> ProcessF i o state is os din dout m ~> ProcessF i o state is os din' dout m
imapFInFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid (f d) next


mapFOutFocus :: forall i o state is os din dout dout' m. (dout -> dout') -> ProcessF i o state is os din dout m ~> ProcessF i o state is os din dout' m
mapFOutFocus f =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid (f d) next
        SendIn iid d next -> SendIn iid d next


imapMInFocus :: forall i o state is os din din' dout m. (din -> din') -> (din' -> din) -> ProcessM i o state is os din dout m ~> ProcessM i o state is os din' dout m
imapMInFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFInFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapMOutFocus :: forall i o state is os din dout dout' m. (dout -> dout') -> ProcessM i o state is os din dout m ~> ProcessM i o state is os din dout' m
mapMOutFocus f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFOutFocus f) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


mapFM :: forall i o state is os din dout m m'. (m ~> m') -> ProcessF i o state is os din dout m ~> ProcessF i o state is os din dout m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next


mapMM :: forall i o state is os din dout m m'. (m ~> m') -> ProcessM i o state is os din dout m ~> ProcessM i o state is os din dout m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


{-
runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree -}




type TestInputs = ( foo :: String, i2 :: Boolean )
type TestOutputs = ( bar :: Int, o2 :: Boolean )


runM
    :: forall i o state is os din dout m
     . MonadEffect m
    => MonadRec m
    => IsSymbol i
    => IsSymbol o
    => Cons i din is is
    => Cons o dout os os
    => Ref (Record is)
    -> Ref (Record os)
    -> Ref state
    -> ProcessM i o state is os din dout m
    ~> m
runM inputsRef outputsRef stateRef (ProcessM processFree) =
    runFreeM inputsRef outputsRef stateRef processFree


-- TODO: pass the inputs / outputs records here, with the current content and so the scheme for types, they can be stored in `Protocol`.
-- runFreeM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> Free (ProcessF state d m) ~> m
-- runFreeM :: forall is os state d m. MonadEffect m => MonadRec m => Row is -> Row os -> d -> Ref state -> Free (ProcessF state d m) ~> m
runFreeM
    :: forall i o state is os din dout m
     . MonadEffect m
    => MonadRec m
    => IsSymbol i
    => IsSymbol o
    => Cons i din is is
    => Cons o dout os os
    => Ref (Record is)
    -> Ref (Record os)
    -> Ref state
    -> Free (ProcessF i o state is os din dout m)
    ~> m
runFreeM inputsRef outputsRef stateRef fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go :: ProcessF i o state is os din dout m ~> m
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
        --getInputAt :: Input i -> m d
        getInputAt iid = liftEffect $ Record.get iid <$> Ref.read inputsRef
        -- sendToOutput :: Output o -> d -> m Unit
        sendToOutput oid v = liftEffect $ Ref.modify_ (Record.set oid v) outputsRef
        -- sendToInput :: Cons i d is is => IsSymbol i => Input i -> d -> m Unit
        sendToInput iid v = liftEffect $ Ref.modify_ (Record.set iid v) inputsRef