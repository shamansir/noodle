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
  , join
  , mkRunner
  , spawn
  )
  where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.List (List)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

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
-- import Noodle.Raw.Fn.Protocol (InletsUpdate, OutletsUpdate) as Raw
import Noodle.Fn.Protocol (Protocol) as Fn
import Noodle.Raw.Fn.Process (ProcessM(..), ProcessF) as Raw
import Noodle.Raw.Fn.Process (imapMState, mapMM, runFreeM, receive, send, sendIn, lift, toReprableState, join, mkRunner, spawn) as Raw
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from, to) as StRepr
import Noodle.Repr.ChRepr (class FromChRepr, class ToChRepr)
import Noodle.Repr.ChRepr (ensureTo, ensureFrom) as ChRepr


newtype ProcessM :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
newtype ProcessM state is os chrepr m a = ProcessM (Raw.ProcessM state chrepr m a)


derive instance Newtype (ProcessM state is os chrepr m a) _
derive newtype instance functorProcessM :: Functor (ProcessM state is os chrepr m)
derive newtype instance applyProcessM :: Apply (ProcessM state is os chrepr m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state is os chrepr m)
derive newtype instance bindProcessM :: Bind (ProcessM state is os chrepr m)
derive newtype instance monadProcessM :: Monad (ProcessM state is os chrepr m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state is os chrepr m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state is os chrepr m a)
derive newtype instance monadEffectProcessM :: MonadEffect m => MonadEffect (ProcessM state is os chrepr m)
derive newtype instance monadAffProcessM :: MonadAff m => MonadAff (ProcessM state is os chrepr m)
derive newtype instance monadStateProcessM :: MonadState state (ProcessM state is os chrepr m)
derive newtype instance monadThrowProcessM :: MonadThrow e m => MonadThrow e (ProcessM state is os chrepr m)
derive newtype instance monadRecProcessM :: MonadRec (ProcessM state is os chrepr m)


type ProcessF :: forall is' os'. Type -> Row is' -> Row os' -> Type -> (Type -> Type) -> Type -> Type
type ProcessF state is os chrepr m a = Raw.ProcessF state chrepr m a


type Process (state :: Type) (is :: Row Type) (os :: Row Type) (chrepr :: Type) (m :: Type -> Type) = ProcessM state is os chrepr m Unit


{- Processing -}

receive :: forall i state is is' os din chrepr m. FromChRepr chrepr din => IsSymbol i => Cons i din is' is => Inlet i -> ProcessM state is os chrepr m din -- RawProcessM state chrepr m din
receive iid =
    wrap $ ChRepr.ensureFrom <$> Raw.receive (inletR iid)
    -- ProcessM $ Free.liftF $ wrap $ Raw.Receive (inletR iid) $ Repr.ensureFrom



send :: forall o state is os os' dout chrepr m. ToChRepr dout chrepr => IsSymbol o => Cons o dout os' os => Outlet o -> dout -> ProcessM state is os chrepr m Unit
send oid dout =
    wrap $ Raw.send (outletR oid) (ChRepr.ensureTo dout)



sendIn :: forall i din state is is' os chrepr m. ToChRepr din chrepr => IsSymbol i => Cons i din is' is => Inlet i -> din -> ProcessM state is os chrepr m Unit
sendIn iid din =
    wrap $ Raw.sendIn (inletR iid) (ChRepr.ensureTo din)


lift :: forall state is os chrepr m. m Unit -> ProcessM state is os chrepr m Unit
lift = wrap <<< Raw.lift


inletsOf :: forall rl is. RL.RowToList is rl => Keys rl => Record is -> List String
inletsOf = keys


outletsOf :: forall rl os. RL.RowToList os rl => Keys rl => Record os -> List String
outletsOf = keys


join :: forall state is os chrepr m a. ProcessM state is os chrepr m a -> ProcessM state is os chrepr m a
join = wrap <<< Raw.join <<< unwrap


mkRunner :: forall state is os chrepr m. HasFallback chrepr => MonadRec m => MonadEffect m => ProcessM state is os chrepr m (ProcessM state is os chrepr m Unit -> m Unit)
mkRunner = wrap $ Raw.mkRunner <#> \runner -> unwrap >>> runner


spawn :: forall state is os chrepr m. HasFallback chrepr => MonadRec m => MonadEffect m => ProcessM state is os chrepr m Unit -> ProcessM state is os chrepr m (m Unit)
spawn = wrap <<< Raw.spawn <<< unwrap


{- Maps -}



imapMState :: forall state state' is os chrepr m. (state -> state') -> (state' -> state) -> ProcessM state is os chrepr m ~> ProcessM state' is os chrepr m
imapMState f g =
    unwrap >>> Raw.imapMState f g >>> wrap


mapMM :: forall state is os chrepr m m'. (m ~> m') -> ProcessM state is os chrepr m ~> ProcessM state is os chrepr m'
mapMM f =
    unwrap >>> Raw.mapMM f >>> wrap



{-
tuplify :: forall ostate istate is os chrepr m. ostate -> ProcessM istate is os chrepr m ~> ProcessM (ostate /\ istate) is os chrepr m
tuplify ostate = imapMState ((/\) ostate) Tuple.snd


untuple :: forall ostate istate is os chrepr m. ostate -> ProcessM (ostate /\ istate) is os chrepr m ~> ProcessM istate is os chrepr m
untuple ostate = imapMState Tuple.snd ((/\) ostate)
-}


{-
runM :: forall i o state d m. MonadEffect m => MonadRec m => Ord i => Protocol i o d -> d -> Ref state -> ProcessM state d m ~> m
runM protocol default stateRef (ProcessM processFree) =
    runFreeM protocol default stateRef processFree -}



runM
    :: forall state is os chrepr m
     . MonadEffect m
    => MonadRec m
    => HasFallback chrepr
    => Fn.Protocol state is os chrepr -- Protocol state is os
    -> ProcessM state is os chrepr m
    ~> m
runM protocol processM =
    case toRaw processM of
        Raw.ProcessM processFree -> runFreeM protocol processFree


runFreeM
    :: forall state is os chrepr m
     . MonadEffect m
    => MonadRec m
    => HasFallback chrepr
    => Fn.Protocol state is os chrepr
    -> Free (Raw.ProcessF state chrepr m)
    ~> m
runFreeM protocol fn = Raw.runFreeM protocol fn


toRaw :: forall state is os m a chrepr. ProcessM state is os chrepr m a -> Raw.ProcessM state chrepr m a
toRaw = unwrap


toRawWithReprableState :: forall state strepr is os m a chrepr. StRepr strepr state => ProcessM state is os chrepr m a -> Raw.ProcessM strepr chrepr m a
toRawWithReprableState = toRaw >>> Raw.toReprableState
