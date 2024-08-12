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
  , inletsOf
  , outletsOf
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)
import Data.List (List)

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

import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet, unsafeSet) as Record
import Unsafe.Coerce (unsafeCoerce)

import Noodle.Id (Inlet, InletR, Outlet, OutletR, inletR, outletR)
import Noodle.Id (inletRName, outletRName) as Id
import Noodle.Fn.Generic.Updates (InletsChange(..), OutletsChange(..))
import Noodle.Fn.Rec.Protocol (Protocol) as Rec



-- FIXME: should not be used anymore, Raw process on Repr-ing looks much better


data ProcessF :: forall is' os'. Type -> Row is' -> Row os' -> (Type -> Type) -> Type -> Type
data ProcessF state is os m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send OutletR (forall dout. dout) a
    | SendIn InletR (forall din. din) a
    | Receive InletR (forall din. din -> a)
    -- | RunEffect (Effect a)
    -- | ToEffect (m a -> Effect a)
    -- | FromEffect (Effect a -> m a)
    -- | LoadFromInlets (forall din. (Record is -> din) /\ (din -> a))
    -- | ModifyInlets (Record is -> Record is)
    -- | ModifyOutlets (Record os -> Record os)
    -- | ReceiveFn (forall din. Record is -> a /\ din)
    -- Connect
    -- Disconnect etc


instance functorProcessF :: Functor m => Functor (ProcessF state is os m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive iid k -> Receive iid (map f k)
        Send oid d next -> Send oid d $ f next
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

receive :: forall i state is is' os din m. IsSymbol i => Cons i din is' is => Inlet i -> ProcessM state is os m din
receive iid = ProcessM $ Free.liftF $ Receive (inletR iid) (unsafeCoerce {-identity-})


send :: forall o state is os os' dout m. IsSymbol o => Cons o dout os' os => Outlet o -> dout -> ProcessM state is os m Unit
-- send oid d = ProcessM $ Free.liftF $ Send' (unsafeCoerce oid /\ unsafeCoerce d) unit
send oid d = ProcessM $ Free.liftF $ Send (outletR oid) (unsafeCoerce d) unit


-- sendIn :: forall i state d m. Inlet i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ i din state is is' os m. IsSymbol i => Cons i din is' is => Inlet i → din → ProcessM state is os m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    -- ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid /\ unsafeCoerce d) unit
    ProcessM $ Free.liftF $ SendIn (inletR iid) (unsafeCoerce d) unit


lift :: forall state is os m. m Unit -> ProcessM state is os m Unit
lift m = ProcessM $ Free.liftF $ Lift m


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


{- Maps -}


imapFState :: forall state state' is os m. (state -> state') -> (state' -> state) -> ProcessF state is os m ~> ProcessF state' is os m
imapFState f g =
    case _ of
        State k -> State (map f <<< k <<< g)
        Lift m -> Lift m
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
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
        Receive iid k -> Receive iid k
        Send oid d next -> Send oid d next
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


-- TODO: pass the inlets / outlets records here, with the current content and so the scheme for types, they can be stored in `Protocol`.
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
        go (Receive iid getV) = do
            valueAtInlet <- getInletAt iid
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
        -- go (RunEffect eff) = do
        --     liftEffect eff
        -- go (ToEffect fn) = do
        --     liftEffect eff
        {-
        go (LoadFromInlets (fn /\ getV)) = do
            valueFromInlets <- loadFromInlets fn
            pure
                $ getV
                $ valueFromInlets
        -}

        getUserState = liftEffect $ protocol.getState unit
        writeUserState _ nextState = liftEffect $ protocol.modifyState $ const nextState
        -- markLastInlet :: InletId -> m Unit
        -- -- markLastInlet iid = protocol.storeLastInlet $ Just $ reifySymbol iid (unsafeCoerce <<< toInlet)
        -- markLastInlet iid = protocol.storeLastInlet $ Just iid
        -- markLastOutlet :: OutletId -> m Unit
        -- markLastOutlet oid = protocol.storeLastOutlet $ Just $ reifySymbol oid (unsafeCoerce <<< toOutlet)
        -- markLastOutlet oid = protocol.storeLastOutlet $ Just oid
        -- getInletAt :: forall i din. IsSymbol i => Cons i din is is => Inlet i -> m din
        -- getInletAt iid = liftEffect $ Record.get iid <$> Ref.read inletsRef
        getInletAt :: forall din. InletR -> m din
        getInletAt iid = liftEffect $ Record.unsafeGet (Id.inletRName iid) <$> Tuple.snd <$> protocol.getInlets unit
        -- loadFromInlets :: forall din. (Record is -> din) -> m din
        -- loadFromInlets fn = fn <$> protocol.getInlets unit
        -- sendToOutlet :: forall o dout. IsSymbol o => Cons o dout os os => Outlet o -> dout -> m Unit
        -- sendToOutlet oid v = liftEffect $ Ref.modify_ (Record.set oid v) outletsRef
        sendToOutlet :: forall dout. OutletR -> dout -> m Unit
        sendToOutlet oid v = liftEffect $ protocol.modifyOutlets $ Record.unsafeSet (Id.outletRName oid) v >>> (Tuple $ SingleOutlet oid) -- Ref.modify_ (Record.unsafeSet oid v) outletsRef
        -- modifyOutlets :: (Record os -> Record os) -> m Unit
        -- modifyOutlets fn = protocol.modifyOutlets fn
        -- sendToInlet :: forall i din. IsSymbol i => Cons i din is is => Inlet i -> din -> m Unit
        -- sendToInlet iid v = liftEffect $ Ref.modify_ (Record.set iid v) inletsRef
        sendToInlet :: forall din. InletR -> din -> m Unit
        sendToInlet iid v = liftEffect $ protocol.modifyInlets $ Record.unsafeSet (Id.inletRName iid) v >>> (Tuple $ SingleInlet iid)
        -- modifyInlets :: (Record is -> Record is) -> m Unit
        -- modifyInlets fn = protocol.modifyInlets fn