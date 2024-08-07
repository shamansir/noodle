module Noodle.Fn.Process
  ( ProcessF(..) -- TODO: hide constructor
  , ProcessM
  , imapMState
  , lift
  , mapMM
  , receive
  , send
  , sendIn
  , inletsOf
  , outletsOf
  , runM
--   , runFreeM
  , toRaw
  )
  where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.List (List)
import Data.Repr (class FromRepr, class HasFallback, class ToRepr)
import Data.Repr (ensureTo, ensureFrom) as Repr
import Data.Newtype (class Newtype, wrap, unwrap)

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

import Noodle.Id (Inlet, Outlet, inletR, outletR)
-- import Noodle.Fn.Raw.Protocol (InletsChange, OutletsChange) as Raw
import Noodle.Fn.Protocol (Protocol) as Fn
import Noodle.Fn.Raw.Process (RawProcessM(..), RawProcessF)
import Noodle.Fn.Raw.Process (RawProcessF(..), imapFState, mapFM, runFreeM) as Raw


newtype ProcessF :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessF state is os repr m a = ProcessF (RawProcessF state repr m a)

derive instance Newtype (ProcessF state is os repr m a) _


newtype ProcessM :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessM state is os repr m a = ProcessM (Free (ProcessF state is os repr m) a)


derive newtype instance functorProcessM :: Functor (ProcessM state is os repr m)
derive newtype instance applyProcessM :: Apply (ProcessM state is os repr m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state is os repr m)
derive newtype instance bindProcessM :: Bind (ProcessM state is os repr m)
derive newtype instance monadProcessM :: Monad (ProcessM state is os repr m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state is os repr m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state is os repr m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state is os m a)


instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state is os repr m) where
  liftEffect = ProcessM <<< Free.liftF <<< ProcessF <<< Raw.Lift <<< liftEffect


instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state is os repr m) where
  liftAff = ProcessM <<< Free.liftF <<< ProcessF <<< Raw.Lift <<< liftAff


instance monadStateProcessM :: MonadState state (ProcessM state is os repr m) where
  state = ProcessM <<< Free.liftF <<< ProcessF <<< Raw.State


instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state is os repr m) where
  throwError = ProcessM <<< Free.liftF <<< ProcessF <<< Raw.Lift <<< throwError


instance monadRecProcessM :: MonadRec (ProcessM state is os repr m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


{- Processing -}


receive :: forall i state is is' os din repr m. FromRepr repr din => IsSymbol i => Cons i din is' is => Inlet i -> ProcessM state is os repr m din
receive iid =
    ProcessM $ Free.liftF $ wrap $ Raw.Receive (inletR iid) $ Repr.ensureFrom


send :: forall o state is os os' dout repr m. ToRepr dout repr => IsSymbol o => Cons o dout os' os => Outlet o -> dout -> ProcessM state is os repr m Unit
send oid d =
    ProcessM $ Free.liftF $ wrap $ Raw.Send (outletR oid) (Repr.ensureTo d) unit


-- sendIn :: forall i state d m. Inlet i -> d -> ProcessM state d m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn iid d unit
sendIn ∷ ∀ i din state is is' os repr m. ToRepr din repr => IsSymbol i => Cons i din is' is => Inlet i → din → ProcessM state is os repr m Unit
-- sendIn iid d = ProcessM $ Free.liftF $ SendIn (unsafeCoerce iid) d unit
sendIn iid d =
    ProcessM $ Free.liftF $ wrap $ Raw.SendIn (inletR iid) (Repr.ensureTo d) unit


lift :: forall state is os repr m. m Unit -> ProcessM state is os repr m Unit
lift m =
    ProcessM $ Free.liftF $ wrap $ Raw.Lift m


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


{- Maps -}


imapFState :: forall state state' is os repr m. (state -> state') -> (state' -> state) -> ProcessF state is os repr m ~> ProcessF state' is os repr m
imapFState f g = unwrap >>> Raw.imapFState f g >>> wrap


imapMState :: forall state state' is os repr m. (state -> state') -> (state' -> state) -> ProcessM state is os repr m ~> ProcessM state' is os repr m
imapMState f g (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< imapFState f g) $ processFree



mapFM :: forall state is os repr m m'. (m ~> m') -> ProcessF state is os repr m ~> ProcessF state is os repr m'
mapFM f = unwrap >>> Raw.mapFM f >>> wrap


mapMM :: forall state is os repr m m'. (m ~> m') -> ProcessM state is os repr m ~> ProcessM state is os repr m'
mapMM f (ProcessM processFree) =
    ProcessM $ foldFree (Free.liftF <<< mapFM f) $ processFree


{-
runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree -}



runM
    :: forall state is os repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Fn.Protocol state is os repr -- Protocol state is os
    -> ProcessM state is os repr m
    ~> m
runM protocol processM =
    case toRaw processM of
        RawProcessM processFree -> runFreeM protocol processFree


runFreeM
    :: forall state is os repr m
     . MonadEffect m
    => MonadRec m
    => HasFallback repr
    => Fn.Protocol state is os repr
    -> Free (RawProcessF state repr m)
    ~> m
runFreeM protocol fn =
    Raw.runFreeM protocol fn


toRaw :: forall state is os m a repr. ProcessM state is os repr m a -> RawProcessM state repr m a
toRaw (ProcessM processFree) = RawProcessM $ foldFree (Free.liftF <<< unwrap) $ processFree
