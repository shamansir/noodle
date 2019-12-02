module Rpd.API.Covered
    -- * Errors Monad
    ( Errors
    , runErrors
    -- * Error Reporting Functions
    , err
    , err1
    --, choice
    , recover
    , recover_
    , mapRecover
    , unrecover
    -- ** Hoisting Functions
    , hoistMaybe
    , hoistEither
    , hoistEither1
    -- * Errors Transformer
    , ErrorsT
    , runErrorsT
    ) where

import Prelude

import Data.Maybe
import Data.Either
import Data.Array (snoc)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (class Bifunctor)

{- inspired by http://hackage.haskell.org/package/hexpr-0.0.0.0/docs/Control-Monad-Errors.html -}



{-| In many error-checking algorithms, it is desireable to report several
    errors rather than simply terminate on detecting the first error.

    Where 'Either' and 'Error' terminates on the first error, 'Errors' can
    recover at specified points and continue error-checking. Even after a
    recovery, the prior errors are logged. If any errors occured during
    error-checking, this si an error in the whole computation.
-}

import Prelude

import Data.Monoid
import Data.Identity
import Data.Either
import Data.Maybe
import Data.Array ((:))
import Data.Array (singleton) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse)
import Control.Applicative
import Control.Monad
import Control.Comonad
import Control.Monad.Writer
import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Either hiding (hoistEither)


{-| Shortcut for 'ErrorsT' over the 'Identity' monad. -}
type Errors e = ErrorsT e Identity
{-| Computations that can collect multiple errors. -}
newtype ErrorsT e m a = ErrorsT (m (Maybe e -> (Maybe a /\ Maybe e)))


{-| Perform an error-reporting computation. -}
runErrors :: forall e a. (Monoid e) => Errors e a -> Either e a
runErrors = extract <<< runErrorsT


{-| Perform the error reporting part of a computation. -}
runErrorsT :: forall e m a. Monad m => Monoid e => ErrorsT e m a -> m (Either e a)
runErrorsT (ErrorsT unErrors) = do
    innerAction <- unErrors
    let res = innerAction Nothing
    pure $ case res of
        (Just val /\ Nothing) -> Right val
        (_ /\ Just errs) -> Left errs
        (Nothing /\ Nothing) -> Left mempty


{-| Report an error. -}
err :: forall e m a. Monad m => Monoid e => e -> ErrorsT e m a
err msg = ErrorsT <<< pure $ \e -> (Nothing /\ (e <> Just msg))

{-| Report one error accumulating in a list. -}
err1 :: forall e m a. Monad m => e -> ErrorsT (Array e) m a
err1 = err <<< Array.singleton

{-| Try several alternatives (in order), but if none succeed, raise the passed error. -}
{-
choice :: Monad m => Monoid e => e -> Array (ErrorsT e m a) -> ErrorsT e m a
choice e0 [] = err e0
choice e0 (a : as) = do
    res <- lift $ runErrorsT a
    case res of
        Left e0 -> choice e0 as
        Right val -> pure val
-}

{-| If the action returns an error, relpace the result with a default.
    The error is still logged and reported at the end of the computation. -}
recover :: forall e m a. Monad m => Monoid e => a -> ErrorsT e m a -> ErrorsT e m a
recover replacement action = ErrorsT $ do
    res <- runErrorsT action
    pure $ case res of
        Left err -> \e -> (Just replacement /\ (e <> Just err))
        Right val -> \e -> (Just val /\ e)

{-| As 'recover', but any successful result value does not matter. -}
recover_ :: forall e m a. Monad m => Monoid e => ErrorsT e m a -> ErrorsT e m Unit
recover_ action = recover unit (const unit <$> action)

{-| Perform many error checks, recovering between each. The value at each index of the output
    list corresponds to the index of the input computation list. Error values are 'Nothing'
    in the output, successful values are wrapped in 'Just'. -}
mapRecover :: forall e m a. Monad m => Monoid e => Array (ErrorsT e m a) -> ErrorsT e m (Array (Maybe a))
mapRecover actions = traverse (recover Nothing <<< ((<$>) Just)) actions

{-| If any errors have been detected, cuase them to be loud again. -}
unrecover :: forall m e. Monad m => Monoid e => ErrorsT e m Unit
unrecover = ErrorsT <<< pure $ \e -> case e of
    Nothing -> (Just unit /\ e)
    Just _ -> (Nothing /\ e)

{-| Turn a 'Maybe' computation into an 'ErrorsT' computation. -}
hoistMaybe :: forall e m a. Monad m => Monoid e => e -> Maybe a -> ErrorsT e m a
hoistMaybe e = maybe (err e) pure

{-| Turn an 'Either' computation into an 'ErrorsT' computation. -}
hoistEither :: forall e m a. Monad m => Monoid e => Either e a -> ErrorsT e m a
hoistEither = either err pure

{-| Turn an 'Either' computation into an 'ErrorsT' computation when accumulating a list. -}
hoistEither1 :: forall e m a. Monad m => Either e a -> ErrorsT (Array e) m a
hoistEither1 = either err1 pure


instance functorErrorsT :: (Monad m, Monoid e) => Functor (ErrorsT e m) where
    map f m = m >>= (pure <<< f)

instance applyErrorsT :: (Monad m, Monoid e) => Apply (ErrorsT e m) where
    apply = ap

instance applicativeErrorsT :: (Monad m, Monoid e) => Applicative (ErrorsT e m) where
    pure v = ErrorsT $ pure $ \e -> (Just v /\ e)

instance bindErrorsT :: (Monad m, Monoid e) => Bind (ErrorsT e m) where
    bind x k = ErrorsT $ do
        xRes <- runErrorsT x
        case xRes of
            Left err -> pure $ \e -> (Nothing /\ (e <> Just err))
            Right val -> let (ErrorsT unErrorsY) = k val in unErrorsY
                -- pure $ \e -> yVal
            -- Right val ->  unErrors $ k val
            -- Right val ->  pure $ (\y err -> ?wh $ runErrorsT y) $ k val
            {-
            Right val -> pure $ \e -> do
                yRes <- runErrorsT $ k val
                pure $ case yRes of
                    Left err' -> \e -> (val /\ (e <> Just err'))
                    Right val' -> \e -> (val' /\ e)
            -}

instance monadErrorsT :: (Monad m, Monoid e) => Monad (ErrorsT e m)

instance monadTransErrorsT :: (Monoid e) => MonadTrans (ErrorsT e) where
    lift x = ErrorsT $ do
        x' <- x
        pure $ \e -> (Just x' /\ e)

-- instance (MonadIO m, Monoid e) => MonadIO (ErrorsT e m) where
--     liftIO = lift . liftIO



data Covered error state =
    Covered (Array error) (Maybe state)


nothing :: forall error state. Covered error state
nothing =
    Covered [] Nothing


uncover :: forall error state. Covered error state -> Array error /\ Maybe state
uncover (Covered errors maybeState) = errors /\ maybeState


notice :: forall error state. error -> Covered error state
notice error = Covered [error] Nothing


cover :: forall error state. state -> Covered error state
cover state = Covered [] $ Just state


-- covered :: forall error state. state -> Covered error state

hoist
    :: forall error state
     . Array error
    -> Covered error state
    -> Covered error state
hoist error (Covered prevErrors maybeState) =
    Covered (prevErrors <> error) maybeState


hoistOne :: forall error state. error -> Covered error state -> Covered error state
hoistOne error (Covered prevErrors maybeState) = Covered (prevErrors `snoc` error) maybeState


coverIn :: forall error state. state -> Covered error state -> Covered error state
coverIn state (Covered errors _) = Covered errors $ Just state


instance functorCovered :: Functor (Covered errors) where
    map f (Covered errors maybeState) = Covered errors $ f <$> maybeState


instance bifunctorCovered :: Bifunctor Covered where
  bimap f g (Covered errors maybeState) = Covered (f <$> errors) (g <$> maybeState)


instance applyCovered :: Apply (Covered errors) where
  apply (Covered errors maybeF) (Covered prevErrors maybeState) =
    Covered (prevErrors <> errors) (maybeF <*> maybeState) -- FIXME: wrong, could not satisfy the law


instance applicativeCovered :: Applicative (Covered errors) where
  pure = cover


instance bindEither :: Bind (Covered errors) where
  bind (Covered errors (Just state)) f = f state
  bind (Covered errors Nothing) _ = Covered errors Nothing


fromEither :: forall error state. Either error state -> Covered error state
fromEither = either notice cover


fromMaybe :: forall error state. Maybe state -> Covered error state
fromMaybe = maybe nothing cover


instance showCovered :: (Show error, Show state) => Show (Covered error state) where
  show (Covered errors maybeState) = "Covered " <> show errors <> " " <> show maybeState


-- coverEither :: forall m errors state error. Monad m => Semigroup errors => Either error state -> Covered errors m state
-- coverEither (Left error) =
--     nothing
