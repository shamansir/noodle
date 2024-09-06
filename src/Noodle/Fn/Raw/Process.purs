module Noodle.Fn.Raw.Process
  ( RawProcessF(..)  -- FIXME: close the constructor
  , RawProcessM(..) -- FIXME: close the constructor
  , RawProcess
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
import Noodle.Fn.Raw.Protocol (Protocol) as Raw



data RawProcessF :: Type -> Type -> (Type -> Type) -> Type -> Type
data RawProcessF state repr m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send OutletR (Repr repr) a
    | SendIn InletR (Repr repr) a
    | Receive InletR (Repr repr -> a)


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


type RawProcess state repr m = RawProcessM state repr m Unit


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


instance monadRecRawProcessM :: MonadRec (RawProcessM state repr m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall state repr m. InletR -> RawProcessM state repr m (Repr repr)
receive inletR =
    RawProcessM $ Free.liftF $ Receive inletR $ identity


send :: forall state repr m. OutletR -> Repr repr -> RawProcessM state repr m Unit
send outletR orepr =
    RawProcessM $ Free.liftF $ Send outletR orepr unit


sendIn ∷ forall state repr m. InletR → Repr repr → RawProcessM state repr m Unit
sendIn inletR irepr =
    RawProcessM $ Free.liftF $ SendIn inletR irepr unit


lift :: forall state repr m. m Unit -> RawProcessM state repr m Unit
lift m = RawProcessM $ Free.liftF $ Lift m


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


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


toReprableState :: forall state repr m. FromRepr repr state => ToRepr state repr => RawProcessM state repr m ~> RawProcessM repr repr m
toReprableState =
    imapMState (ensureTo >>> unwrap) (ensureFrom <<< wrap)


{- Running -}


runM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Raw.Protocol state repr
    -> RawProcessM state repr m
    ~> m
runM protocol (RawProcessM processFree) =
    runFreeM protocol processFree


runFreeM
    :: forall state repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Raw.Protocol state repr
    -> Free (RawProcessF state repr m)
    ~> m
runFreeM protocol fn =
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