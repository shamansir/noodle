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
  , join
  , mkRunner
  , spawn
  , initial
--   , getProtocol -- TODO: is it ok to expose?
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)

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
import Noodle.Id (inletRName) as Id
import Noodle.Fn.Generic.Updates (InletsUpdate(..), OutletsUpdate(..))
import Noodle.Raw.Fn.Protocol (Protocol) as Raw
import Noodle.Fn.Generic.Protocol (imapState) as RawProtocol
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (ensureFrom, to) as StRepr
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (_missingKey) as ViC
-- import Noodle.Repr.ChRepr (ChRepr, class ToChRepr, class FromChRepr, fallbackByChRepr)
-- import Noodle.Repr.ChRepr (unwrap, wrap, ensureTo, ensureFrom) as ChRepr



data ProcessF :: Type -> Type -> (Type -> Type) -> Type -> Type
data ProcessF state chrepr m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Join (ProcessF state chrepr m a)
    | GetProto (Raw.Protocol state chrepr -> a)
    | Send    OutletR (ValueInChannel chrepr) a
    | SendIn  InletR  (ValueInChannel chrepr) a
    | Receive InletR  (ValueInChannel chrepr -> a)


instance functorProcessF :: Functor m => Functor (ProcessF state chrepr m) where
    map f = case _ of
        Join proc ->         Join $ map f proc
        GetProto pf ->       GetProto $ map f pf
        State k ->           State $ lmap f <<< k
        Lift m ->            Lift $ map f m
        Receive iid k ->     Receive iid $ map f k
        Send oid d next ->   Send oid d $ f next
        SendIn iid d next -> SendIn iid d $ f next
        -- RunEffect effA -> RunEffect $ map f effA


newtype ProcessM :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessM state chrepr m a = ProcessM (Free (ProcessF state chrepr m) a)


type Process state chrepr m = ProcessM state chrepr m Unit


derive newtype instance functorProcessM :: Functor (ProcessM state chrepr m)
derive newtype instance applyProcessM :: Apply (ProcessM state chrepr m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state chrepr m)
derive newtype instance bindProcessM :: Bind (ProcessM state chrepr m)
derive newtype instance monadProcessM :: Monad (ProcessM state chrepr m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state chrepr m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state chrepr m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state chrepr m a)


instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state chrepr m) where
  liftEffect = ProcessM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state chrepr m) where
  liftAff = ProcessM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateProcessM :: MonadState state (ProcessM state chrepr m) where
  state = ProcessM <<< Free.liftF <<< State


instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state chrepr m) where
  throwError = ProcessM <<< Free.liftF <<< Lift <<< throwError


instance monadRecProcessM :: MonadRec (ProcessM state chrepr m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}

receive :: forall state chrepr m. InletR -> ProcessM state chrepr m (ValueInChannel chrepr)
receive inletR =
    ProcessM $ Free.liftF $ Receive inletR $ identity


send :: forall state chrepr m. OutletR -> ValueInChannel chrepr -> ProcessM state chrepr m Unit
send outletR orepr =
    ProcessM $ Free.liftF $ Send outletR orepr unit


sendIn ∷ forall state chrepr m. InletR -> ValueInChannel chrepr -> ProcessM state chrepr m Unit
sendIn inletR irepr =
    ProcessM $ Free.liftF $ SendIn inletR irepr unit


lift :: forall state chrepr m. m Unit -> ProcessM state chrepr m Unit
lift m = ProcessM $ Free.liftF $ Lift m


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


join :: forall state chrepr m a. ProcessM state chrepr m a -> ProcessM state chrepr m a
join (ProcessM free) = ProcessM $ Free.hoistFree Join free


getProtocol :: forall state chrepr m. ProcessM state chrepr m (Raw.Protocol state chrepr)
getProtocol = ProcessM $ Free.liftF $ GetProto identity


initial :: forall state chrepr m. ProcessM state chrepr m { state :: state, inlets :: Map InletR (ValueInChannel chrepr), outlets :: Map OutletR (ValueInChannel chrepr) }
initial = getProtocol >>= (_.initial >>> pure)


mkRunner :: forall state chrepr m. HasFallback chrepr => MonadRec m => MonadEffect m => ProcessM state chrepr m (ProcessM state chrepr m Unit -> m Unit)
mkRunner = getProtocol >>= (pure <<< runM)


spawn :: forall state chrepr m. HasFallback chrepr => MonadRec m => MonadEffect m => ProcessM state chrepr m Unit -> ProcessM state chrepr m (m Unit)
spawn proc = mkRunner <#> (#) proc


{- Maps -}


imapFState :: forall state state' chrepr m. (state -> state') -> (state' -> state) -> ProcessF state chrepr m ~> ProcessF state' chrepr m
imapFState f g =
    case _ of
        Join proc -> Join $ imapFState f g proc
        GetProto pf -> GetProto $ pf <<< RawProtocol.imapState g f
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


imapMState :: forall state state' chrepr m. (state -> state') -> (state' -> state) -> ProcessM state chrepr m ~> ProcessM state' chrepr m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) processFree


mapFM :: forall state chrepr m m'. (m ~> m') -> ProcessF state chrepr m ~> ProcessF state chrepr m'
mapFM f =
    case _ of
        Join proc -> mapFM f proc
        GetProto pf -> GetProto pf
        State k -> State k
        Lift m -> Lift $ f m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
        SendIn iid d next -> SendIn iid d next
        -- RunEffect effA -> RunEffect effA


mapMM :: forall state chrepr m m'. (m ~> m') -> ProcessM state chrepr m ~> ProcessM state chrepr m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) processFree


toReprableState :: forall state strepr chrepr m. HasFallback state => StRepr state strepr => ProcessM state chrepr m ~> ProcessM strepr chrepr m
toReprableState =
    imapMState StRepr.to StRepr.ensureFrom


{- Running -}


runM
    :: forall state chrepr m
     . MonadEffect m
    => MonadRec m
    => HasFallback chrepr
    => Raw.Protocol state chrepr
    -> ProcessM state chrepr m
    ~> m
runM protocol (ProcessM processFree) =
    runFreeM protocol processFree


runFreeM
    :: forall state chrepr m
     . MonadEffect m
    => MonadRec m
    => HasFallback chrepr
    => Raw.Protocol state chrepr
    -> Free (ProcessF state chrepr m)
    ~> m
runFreeM protocol fn =
    --foldFree go-- (go stateRef)
    Free.runFreeM go fn
    where
        go :: forall a. ProcessF state chrepr m a -> m a
        go (Join proc) = do
            runFreeM protocol $ Free.liftF $ proc
        go (GetProto getProto) = do
            pure $ getProto protocol
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
        trackMissingInlet :: InletR -> Maybe (ValueInChannel chrepr) -> ValueInChannel chrepr
        trackMissingInlet key = case _ of
            Just viC -> viC
            Nothing -> ViC._missingKey $ Id.inletRName key
        getInletAt :: InletR -> m (ValueInChannel chrepr)
        getInletAt iid = liftEffect $ trackMissingInlet iid <$> Map.lookup iid <$> Tuple.snd <$> protocol.getInlets unit
        sendToOutlet :: OutletR -> ValueInChannel chrepr -> m Unit
        sendToOutlet oid v = liftEffect $ protocol.modifyOutlets $ Map.insert oid v >>> (Tuple $ SingleOutlet oid)
        sendToInlet :: InletR -> ValueInChannel chrepr -> m Unit
        sendToInlet iid v = liftEffect $ protocol.modifyInlets $ Map.insert iid v >>> (Tuple $ SingleInlet iid)