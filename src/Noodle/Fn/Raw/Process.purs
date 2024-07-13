module Noodle.Fn.Raw.Process
  ( RawProcessF(..)  -- FIXME: close the constructor
  , RawProcessM(..) -- FIXME: close the constructor
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
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.List (List)
import Data.SProxy (reflect')
import Data.Repr (class ToRepr, class FromRepr, fromRepr, toRepr)
import Data.Repr (Repr(..), unwrap)
import Data.Traversable (traverse)

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
import Noodle.Fn.Raw.Protocol (InputChange(..), OutputChange(..), RawProtocol)



data RawProcessF :: Type -> Type -> (Type -> Type) -> Type -> Type
data RawProcessF state repr m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send OutputR (Repr repr) a
    | SendIn InputR (Repr repr) a
    | Receive InputR (Repr repr -> a)


instance functorProcessF :: Functor m => Functor (RawProcessF state repr m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive iid k -> Receive iid (map f k)
        Send oid d next -> Send oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next
        -- RunEffect effA -> RunEffect $ map f effA


newtype RawProcessM :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype RawProcessM state repr m a = RawProcessM (Free (RawProcessF state repr m) a)


derive newtype instance functorRawProcessM :: Functor (RawProcessM state repr m)
derive newtype instance applyRawProcessM :: Apply (RawProcessM state repr m)
derive newtype instance applicativeRawProcessM :: Applicative (RawProcessM state repr m)
derive newtype instance bindRawProcessM :: Bind (RawProcessM state repr m)
derive newtype instance monadRawProcessM :: Monad (RawProcessM state repr m)
derive newtype instance semigroupRawProcessM :: Semigroup a => Semigroup (RawProcessM state repr m a)
derive newtype instance monoidRawProcessM :: Monoid a => Monoid (RawProcessM state repr m a)
--derive newtype instance bifunctorRawProcessM :: Bifunctor (RawProcessM state repr m a)


instance monadEffectRawProcessM :: MonadEffect m => MonadEffect (RawProcessM state repr m) where
  liftEffect = RawProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffRawProcessM :: MonadAff m => MonadAff (RawProcessM state repr m) where
  liftAff = RawProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateRawProcessM :: MonadState state (RawProcessM state repr m) where
  state = RawProcessM <<< Free.liftF <<< State


instance monadThrowRawProcessM :: MonadThrow e m => MonadThrow e (RawProcessM state repr m) where
  throwError = RawProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecProcessM :: MonadRec (RawProcessM state repr m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall state repr m. InputR -> RawProcessM state repr m (Repr repr)
receive inputR =
    RawProcessM $ Free.liftF $ Receive inputR $ identity


send :: forall state repr m. OutputR -> Repr repr -> RawProcessM state repr m Unit
send outputR orepr =
    RawProcessM $ Free.liftF $ Send outputR orepr unit


sendIn ∷ forall state repr m. InputR → Repr repr → RawProcessM state repr m Unit
sendIn inputR irepr =
    RawProcessM $ Free.liftF $ SendIn inputR irepr unit


lift :: forall state repr m. m Unit -> RawProcessM state repr m Unit
lift m = RawProcessM $ Free.liftF $ Lift m


inputsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inputsOf = keys


outputsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outputsOf = keys


{- Maps -}


imapFState :: forall state state' repr m. (state -> state') -> (state' -> state) -> RawProcessF state repr m ~> RawProcessF state' repr m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


imapMState :: forall state state' repr m. (state -> state') -> (state' -> state) -> RawProcessM state repr m ~> RawProcessM state' repr m
imapMState f g (RawProcessM processFree) =
    RawProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


mapFM :: forall state repr m m'. (m ~> m') -> RawProcessF state repr m ~> RawProcessF state repr m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


mapMM :: forall state repr m m'. (m ~> m') -> RawProcessM state repr m ~> RawProcessM state repr m'
mapMM f (RawProcessM processFree) =
    RawProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


runM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => repr
    -> RawProtocol state repr
    -> RawProcessM state repr m
    ~> m
runM default protocol (RawProcessM processFree) =
    runFreeM default protocol processFree


runFreeM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => repr
    -> RawProtocol state repr
    -> Free (RawProcessF state repr m)
    ~> m
runFreeM default protocol fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go :: forall a. RawProcessF state repr m a -> m a
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next
        go (Lift m) = m
        go (Receive iid getV) = do
            valueAtInput <- getInputAt iid
            -- if there's is no value, throwError ?wh
            pure
                $ getV
                $ valueAtInput

        go (Send oid v next) = do
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
        getInputAt :: InputR -> m (Repr repr)
        getInputAt iid = liftEffect $ maybe (Repr default) Repr <$> Map.lookup iid <$> Tuple.snd <$> protocol.getInputs unit
        sendToOutput :: OutputR -> Repr repr -> m Unit
        sendToOutput oid v = liftEffect $ protocol.modifyOutputs $ Map.insert oid (unwrap v) >>> (Tuple $ SingleOutput oid) -- Ref.modify_ (Record.unsafeSet oid v) outputsRef
        sendToInput :: InputR -> Repr repr -> m Unit
        sendToInput iid v = liftEffect $ protocol.modifyInputs $ Map.insert iid (unwrap v) >>> (Tuple $ SingleInput iid)
