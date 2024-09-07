module Noodle.Fn.Process
  ( Process
  , ProcessM(..)
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
  , toRawWithReprableState
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

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free (Free)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
-- import Noodle.Fn.Protocol (Protocol)

import Prim.Row (class Cons)

import Noodle.Id (Inlet, Outlet, inletR, outletR)
-- import Noodle.Fn.Raw.Protocol (InletsUpdate, OutletsUpdate) as Raw
import Noodle.Fn.Protocol (Protocol) as Fn
import Noodle.Fn.Raw.Process (RawProcessM(..), RawProcessF)
import Noodle.Fn.Raw.Process (imapMState, mapMM, runFreeM, receive, send, sendIn, lift, toReprableState) as Raw


newtype ProcessM :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessM state is os repr m a = ProcessM (RawProcessM state repr m a)


derive instance Newtype (ProcessM state is os repr m a) _
derive newtype instance functorProcessM :: Functor (ProcessM state is os repr m)
derive newtype instance applyProcessM :: Apply (ProcessM state is os repr m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state is os repr m)
derive newtype instance bindProcessM :: Bind (ProcessM state is os repr m)
derive newtype instance monadProcessM :: Monad (ProcessM state is os repr m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state is os repr m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state is os repr m a)
derive newtype instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state is os repr m)
derive newtype instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state is os repr m)
derive newtype instance monadStateProcessM :: MonadState state (ProcessM state is os repr m)
derive newtype instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state is os repr m)
derive newtype instance monadRecProcessM :: MonadRec (ProcessM state is os repr m)


type ProcessF :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
type ProcessF state is os repr m a = RawProcessF state repr m a


type Process (state :: Type) (is :: Row Type) (os :: Row Type) (repr :: Type) (m :: Type -> Type) = ProcessM state is os repr m Unit


{- Processing -}

receive :: forall i state is is' os din repr m. FromRepr repr din => IsSymbol i => Cons i din is' is => Inlet i -> ProcessM state is os repr m din -- RawProcessM state repr m din
receive iid =
    wrap $ Repr.ensureFrom <$> Raw.receive (inletR iid)
    -- ProcessM $ Free.liftF $ wrap $ Raw.Receive (inletR iid) $ Repr.ensureFrom



send :: forall o state is os os' dout repr m. ToRepr dout repr => IsSymbol o => Cons o dout os' os => Outlet o -> dout -> ProcessM state is os repr m Unit
send oid dout =
    wrap $ Raw.send (outletR oid) (Repr.ensureTo dout)



sendIn :: forall i din state is is' os repr m. ToRepr din repr => IsSymbol i => Cons i din is' is => Inlet i -> din -> ProcessM state is os repr m Unit
sendIn iid din =
    wrap $ Raw.sendIn (inletR iid) (Repr.ensureTo din)


lift :: forall state is os repr m. m Unit -> ProcessM state is os repr m Unit
lift = wrap <<< Raw.lift


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


{- Maps -}


imapMState :: forall state state' is os repr m. (state -> state') -> (state' -> state) -> ProcessM state is os repr m ~> ProcessM state' is os repr m
imapMState f g =
    unwrap >>> Raw.imapMState f g >>> wrap


mapMM :: forall state is os repr m m'. (m ~> m') -> ProcessM state is os repr m ~> ProcessM state is os repr m'
mapMM f =
    unwrap >>> Raw.mapMM f >>> wrap


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
runFreeM protocol fn = Raw.runFreeM protocol fn


toRaw :: forall state is os m a repr. ProcessM state is os repr m a -> RawProcessM state repr m a
toRaw = unwrap


toRawWithReprableState :: forall state is os m a repr. FromRepr repr state => ToRepr state repr => ProcessM state is os repr m a -> RawProcessM repr repr m a
toRawWithReprableState = toRaw >>> Raw.toReprableState
