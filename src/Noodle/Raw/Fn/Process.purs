module Noodle.Raw.Fn.Process
  ( ProcessF(..)  -- FIXME: close the constructor
  , ProcessM(..) -- FIXME: close the constructor
  , Process
  , imapFState
  , imapMState
  , lift
  , mapFM
  , mapMM
  , toReprableState
  , receive
  , runFreeM
  , runM
  , send
  , sendIn
  , inletsOf
  , outletsOf
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)
import Data.Repr (class HasFallback, fallbackByRepr, Repr, unwrap, wrap, class ToRepr, class FromRepr, ensureTo, ensureFrom)

import Prim.RowList as RL
import Record.Extra (class Keys, keys)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
-- import Noodle.Fn.Protocol (Protocol)


import Noodle.Id (InletR, OutletR)
import Noodle.Fn.Generic.Updates (InletsUpdate(..), OutletsUpdate(..))
import Noodle.Raw.Fn.Protocol (Protocol) as Raw



data ProcessF :: Type -> Type -> (Type -> Type) -> Type -> Type
data ProcessF state repr m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send OutletR (Repr repr) a
    | SendIn InletR (Repr repr) a
    | Receive InletR (Repr repr -> a)


instance functorProcessF :: Functor m => Functor (ProcessF state repr m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive iid k -> Receive iid (map f k)
        Send oid d next -> Send oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next
        -- RunEffect effA -> RunEffect $ map f effA


newtype ProcessM :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessM state repr m a = ProcessM (Free (ProcessF state repr m) a)


type Process state repr m = ProcessM state repr m Unit


derive newtype instance functorProcessM :: Functor (ProcessM state repr m)
derive newtype instance applyProcessM :: Apply (ProcessM state repr m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state repr m)
derive newtype instance bindProcessM :: Bind (ProcessM state repr m)
derive newtype instance monadProcessM :: Monad (ProcessM state repr m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state repr m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state repr m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state repr m a)


instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state repr m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state repr m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateProcessM :: MonadState state (ProcessM state repr m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state repr m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecProcessM :: MonadRec (ProcessM state repr m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall state repr m. InletR -> ProcessM state repr m (Repr repr)
receive inletR =
    ProcessM $ Free.liftF $ Receive inletR $ identity


send :: forall state repr m. OutletR -> Repr repr -> ProcessM state repr m Unit
send outletR orepr =
    ProcessM $ Free.liftF $ Send outletR orepr unit


sendIn ∷ forall state repr m. InletR → Repr repr → ProcessM state repr m Unit
sendIn inletR irepr =
    ProcessM $ Free.liftF $ SendIn inletR irepr unit


lift :: forall state repr m. m Unit -> ProcessM state repr m Unit
lift m = ProcessM $ Free.liftF $ Lift m


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


{- Maps -}


imapFState :: forall state state' repr m. (state -> state') -> (state' -> state) -> ProcessF state repr m ~> ProcessF state' repr m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


imapMState :: forall state state' repr m. (state -> state') -> (state' -> state) -> ProcessM state repr m ~> ProcessM state' repr m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


mapFM :: forall state repr m m'. (m ~> m') -> ProcessF state repr m ~> ProcessF state repr m'
mapFM f =
    case _ of
        State k -> State k
        Lift m -> Lift $ f m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


mapMM :: forall state repr m m'. (m ~> m') -> ProcessM state repr m ~> ProcessM state repr m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


toReprableState :: forall state repr m. FromRepr repr state => ToRepr state repr => ProcessM state repr m ~> ProcessM repr repr m
toReprableState =
    imapMState (ensureTo >>> unwrap) (ensureFrom <<< wrap)


{- Running -}


runM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Raw.Protocol state repr
    -> ProcessM state repr m
    ~> m
runM protocol (ProcessM processFree) =
    runFreeM protocol processFree


runFreeM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Raw.Protocol state repr
    -> Free (ProcessF state repr m)
    ~> m
runFreeM protocol fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go :: forall a. ProcessF state repr m a -> m a
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next
        go (Lift m) = m
        go (Receive iid getV) = do
            valueAtInlet <- getInletAt iid
            -- if there's is no value, throwError ?wh
            pure
                $ getV
                $ valueAtInlet

        go (Send oid v next) = do
            -- markLastOutlet oid
            -- liftEffect $ Ref.write (reifySymbol oid unsafeCoerce) lastOutletRef
            sendToOutlet oid v
            pure next
        go (SendIn iid v next) = do
            -- markLastInlet iid
            sendToInlet iid v
            pure next

        getUserState = liftEffect $ protocol.getState unit
        writeUserState _ nextState = liftEffect $ protocol.modifyState $ const nextState
        getInletAt :: InletR -> m (Repr repr)
        getInletAt iid = liftEffect $ fallbackByRepr <$> Map.lookup iid <$> Tuple.snd <$> protocol.getInlets unit
        sendToOutlet :: OutletR -> Repr repr -> m Unit
        sendToOutlet oid v = liftEffect $ protocol.modifyOutlets $ Map.insert oid (unwrap v) >>> (Tuple $ SingleOutlet oid)
        sendToInlet :: InletR -> Repr repr -> m Unit
        sendToInlet iid v = liftEffect $ protocol.modifyInlets $ Map.insert iid (unwrap v) >>> (Tuple $ SingleInlet iid)