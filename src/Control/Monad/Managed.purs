-- source: https://github.com/xgrommx/purescript-managed/tree/master/src/Control/Monad

module Control.Monad.Managed where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Effect.Class (class MonadEffect, liftEffect)

newtype Managed a = Managed(forall r e. MonadEffect e => (a -> e r) -> e r)

unManaged :: forall a. Managed a -> (forall r e. MonadEffect e => (a -> e r) -> e r)
unManaged (Managed a) = a

infixl 1 unManaged as >>-

instance functorManaged :: Functor Managed where
  map f mx = Managed (\pure_ -> mx >>- \x -> pure_ (f x))

instance applyManaged :: Apply Managed where
  apply mf mx = Managed (\pure_ -> mf >>- \f -> mx >>- \x -> pure_ (f x))

instance applicativeManaged :: Applicative Managed where
  pure r = Managed (\pure_ -> pure_ r)

instance bindManaged :: Bind Managed where
  bind ma f = Managed (\pure_ -> ma >>- \a -> f a >>- \b -> pure_ b)

instance monadManaged :: Monad Managed

instance monadEffectManaged :: MonadEffect Managed where
  liftEffect m = Managed (\pure_ -> (liftEffect m) >>= pure_)

instance semigroupManaged :: Semigroup a => Semigroup (Managed a) where
  append = lift2 append

instance monoidManaged :: Monoid a => Monoid (Managed a) where
  mempty = pure mempty

instance semiringManaged :: Semiring a => Semiring (Managed a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance ringManaged :: Ring a => Ring (Managed a) where
  sub = lift2 sub

class MonadEffect m <= MonadManaged m where
  using :: forall a. Managed a -> m a

instance monadManagedManaged :: MonadManaged Managed where
  using = identity

instance monadManagedContT :: MonadManaged m => MonadManaged (ContT r m) where
  using m = lift (using m)

instance monadManagedExcepT :: MonadManaged m => MonadManaged (ExceptT e m) where
  using m = lift (using m)

instance monadManagedMaybeT :: MonadManaged m => MonadManaged (MaybeT m) where
  using m = lift (using m)

instance monadManagedReaderT :: MonadManaged m => MonadManaged (ReaderT r m) where
  using m = lift (using m)

instance monadManagedRWST :: (Monoid w, MonadManaged m) => MonadManaged (RWST r w s m) where
  using m = lift (using m)

instance monadManagedStateT :: MonadManaged m => MonadManaged (StateT s m) where
  using m = lift (using m)

instance monadManagedWriterT :: (Monoid w, MonadManaged m) => MonadManaged (WriterT w m) where
  using m = lift (using m)

managed :: forall a. (forall r e. MonadEffect e => (a -> e r) -> e r) -> Managed a
managed x = Managed x

managed_ :: (forall e r. MonadEffect e => e r -> e r) -> Managed Unit
managed_ f = managed (\g -> f $ g unit)

with :: forall a. Managed a -> (forall r e. MonadEffect e => (a -> e r) -> e r)
with m k = m >>- k

runManaged :: forall e. MonadEffect e => Managed Unit -> e Unit
runManaged m = m >>- pure
