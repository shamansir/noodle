module Noodle.Fn.Rec.Process
  ( ProcessF
  , ProcessM
  , imapFState
  , imapMState
  , lift
  , mapFM
  , mapMM
  , receive
  , runFreeM
  , runM
  , send
  , sendIn
  , inputsOf
  , outputsOf
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.List (List)
import Data.SProxy (reflect')

import Prim.RowList as RL
import Record.Extra (class Keys, keys)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)

import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Console as Console
-- import Noodle.Fn.Protocol (Protocol)

import Prim.Row (class Cons)
import Record as Record
import Record.Unsafe (unsafeGet, unsafeSet, unsafeDelete) as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import Noodle.Id (Input, InputR, Output, OutputR, inputR, outputR)
import Noodle.Fn.Generic.Protocol (InputChange(..), OutputChange(..))
import Noodle.Fn.Rec.Protocol (Protocol) as Rec



-- FIXME: should not be used anymore, Raw process on Repr-ing looks much better


data ProcessF :: forall is' os'. Type -> Row is' -> Row os' -> (Type -> Type) -> Type -> Type
data ProcessF state is os m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' OutputR (forall dout. dout) a
    | SendIn InputR (forall din. din) a
    | Receive' InputR (forall din. din -> a)
    -- | RunEffect (Effect a)
    -- | ToEffect (m a -> Effect a)
    -- | FromEffect (Effect a -> m a)
    -- | LoadFromInputs (forall din. (Record is -> din) /\ (din -> a))
    -- | ModifyInputs (Record is -> Record is)
    -- | ModifyOutputs (Record os -> Record os)
    -- | ReceiveFn (forall din. Record is -> a /\ din)
    -- Connect
    -- Disconnect etc


instance functorProcessF :: Functor m => Functor (ProcessF state is os m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid (map f k)
        Send' oid d next -> Send' oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next
        -- RunEffect effA -> RunEffect $ map f effA


newtype ProcessM :: forall is' os'. Type -> Row is' -> Row os' -> (Type -> Type) -> Type -> Type
newtype ProcessM state is os m a = ProcessM (Free (ProcessF state is os m) a)


derive newtype instance functorProcessM :: Functor (ProcessM state is os m)
derive newtype instance applyProcessM :: Apply (ProcessM state is os m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state is os m)
derive newtype instance bindProcessM :: Bind (ProcessM state is os m)
derive newtype instance monadProcessM :: Monad (ProcessM state is os m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state is os m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state is os m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state is os m a)


instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state is os m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state is os m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateProcessM :: MonadState state (ProcessM state is os m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state is os m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecProcessM :: MonadRec (ProcessM state is os m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall i state is is' os din m. IsSymbol i => Cons i din is' is => Input i -> ProcessM state is os m din
receive iid = ProcessM $ Free.liftF $ Receive' (inputR iid) (unsafeCoerce {-identity-})


send :: forall o state is os os' dout m. IsSymbol o => Cons o dout os' os => Output o -> dout -> ProcessM state is os m Unit
-- send oid d = ProcessM $ Free.liftF $ Send' (unsafeCoerce oid /\ unsafeCoerce d) unit
send oid d = ProcessM $ Free.liftF $ Send' (outputR oid) (unsafeCoerce d) unit


-- sendIn :: forall i state d m. Input i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ i din state is is' os m. IsSymbol i => Cons i din is' is => Input i → din → ProcessM state is os m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    -- ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid /\ unsafeCoerce d) unit
    ProcessM $ Free.liftF $ SendIn (inputR iid) (unsafeCoerce d) unit


lift :: forall state is os m. m Unit -> ProcessM state is os m Unit
lift m = ProcessM $ Free.liftF $ Lift m


inputsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inputsOf = keys


outputsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outputsOf = keys


{- Maps -}


imapFState :: forall state state' is os m. (state -> state') -> (state' -> state) -> ProcessF state is os m ~> ProcessF state' is os m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


imapMState :: forall state state' is os m. (state -> state') -> (state' -> state) -> ProcessM state is os m ~> ProcessM state' is os m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree



mapFM :: forall state is os m m'. (m ~> m') -> ProcessF state is os m ~> ProcessF state is os m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive' iid k -> Receive' iid k
        Send' oid d next -> Send' oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


mapMM :: forall state is os m m'. (m ~> m') -> ProcessM state is os m ~> ProcessM state is os m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


{-
runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree -}



runM
    :: forall state is os m
     . MonadEffect m
    => MonadRec m
    => Rec.Protocol state is os
    -> ProcessM state is os m
    ~> m
runM protocol (ProcessM processFree) =
    runFreeM protocol processFree


-- TODO: pass the inputs / outputs records here, with the current content and so the scheme for types, they can be stored in `Protocol`.
-- runFreeM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> Free (ProcessF state d m) ~> m
-- runFreeM :: forall state is os d m. MonadEffect m => MonadRec m => Row is -> Row os -> d -> Ref state -> Free (ProcessF state d m) ~> m
runFreeM
    :: forall state is os m
     . MonadEffect m
    => MonadRec m
    => Rec.Protocol state is os
    -> Free (ProcessF state is os m)
    ~> m
runFreeM protocol fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go :: forall a. ProcessF state is os m a -> m a
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next
        go (Lift m) = m
        go (Receive' iid getV) = do
            valueAtInput <- getInputAt iid
            pure
                $ getV
                $ valueAtInput

        go (Send' oid v next) = do
            -- markLastOutput oid
            -- liftEffect $ Ref.write (reifySymbol oid unsafeCoerce) lastOutputRef
            sendToOutput oid v
            pure next
        go (SendIn iid v next) = do
            -- markLastInput iid
            sendToInput iid v
            pure next
        -- go (RunEffect eff) = do
        --     liftEffect eff
        -- go (ToEffect fn) = do
        --     liftEffect eff
        {-
        go (LoadFromInputs (fn /\ getV)) = do
            valueFromInputs <- loadFromInputs fn
            pure
                $ getV
                $ valueFromInputs
        -}

        getUserState = liftEffect $ protocol.getState unit
        writeUserState _ nextState = liftEffect $ protocol.modifyState $ const nextState
        -- markLastInput :: InputId -> m Unit
        -- -- markLastInput iid = protocol.storeLastInput $ Just $ reifySymbol iid (unsafeCoerce <<< toInput)
        -- markLastInput iid = protocol.storeLastInput $ Just iid
        -- markLastOutput :: OutputId -> m Unit
        -- markLastOutput oid = protocol.storeLastOutput $ Just $ reifySymbol oid (unsafeCoerce <<< toOutput)
        -- markLastOutput oid = protocol.storeLastOutput $ Just oid
        -- getInputAt :: forall i din. IsSymbol i => Cons i din is is => Input i -> m din
        -- getInputAt iid = liftEffect $ Record.get iid <$> Ref.read inputsRef
        getInputAt :: forall din. InputR -> m din
        getInputAt iid = liftEffect $ Record.unsafeGet (reflect' iid) <$> Tuple.snd <$> protocol.getInputs unit
        -- loadFromInputs :: forall din. (Record is -> din) -> m din
        -- loadFromInputs fn = fn <$> protocol.getInputs unit
        -- sendToOutput :: forall o dout. IsSymbol o => Cons o dout os os => Output o -> dout -> m Unit
        -- sendToOutput oid v = liftEffect $ Ref.modify_ (Record.set oid v) outputsRef
        sendToOutput :: forall dout. OutputR -> dout -> m Unit
        sendToOutput oid v = liftEffect $ protocol.modifyOutputs $ Record.unsafeSet (reflect' oid) v >>> (Tuple $ SingleOutput oid) -- Ref.modify_ (Record.unsafeSet oid v) outputsRef
        -- modifyOutputs :: (Record os -> Record os) -> m Unit
        -- modifyOutputs fn = protocol.modifyOutputs fn
        -- sendToInput :: forall i din. IsSymbol i => Cons i din is is => Input i -> din -> m Unit
        -- sendToInput iid v = liftEffect $ Ref.modify_ (Record.set iid v) inputsRef
        sendToInput :: forall din. InputR -> din -> m Unit
        sendToInput iid v = liftEffect $ protocol.modifyInputs $ Record.unsafeSet (reflect' iid) v >>> (Tuple $ SingleInput iid)
        -- modifyInputs :: (Record is -> Record is) -> m Unit
        -- modifyInputs fn = protocol.modifyInputs fn