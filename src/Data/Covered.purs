module Data.Covered where


import Prelude

import Data.Either
import Data.Maybe
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))


data Covered e a
    = Recovered e a
    | Carried a


instance coveredFunctor :: Functor (Covered e) where
    map f (Recovered err a) = Recovered err $ f a
    map f (Carried a) = Carried $ f a


-- instance coveredBifunctor :: Bifunctor Covered where
--     bimap g f (Recovered err a) = Recovered (g err) (f a)
--     bimap _ f (Carried a) = Carried a -- hides the error type


instance coveredApply :: Apply (Covered e) where
    apply (Recovered _ f) covered = f <$> covered
    apply (Carried f) covered = f <$> covered


instance coveredApplicative :: Applicative (Covered e) where
    pure = Carried


instance coveredBind :: Bind (Covered e) where
    bind covered k = k $ recover covered


instance coveredMonad :: Monad (Covered e)


-- TODO: Comonad


carry :: forall e a. a -> Covered e a
carry = Carried


cover :: forall e a. a -> e -> Covered e a
cover x err = Recovered err x


cover' :: forall e a. a -> Maybe e -> Covered e a
cover' x (Just err) = Recovered err x
cover' x Nothing = Carried x


inject :: forall e a b. b -> Covered e a -> Covered e b
inject b covered = const b <$> covered


fromEither :: forall e a. a -> Either e a -> Covered e a
fromEither v either = fromEither' (const v) either


fromEither' :: forall e a.  (Unit -> a) -> Either e a ->Covered e a
fromEither' _ (Right v) = Carried v
fromEither' fallback (Left err)  = Recovered err $ fallback unit


toEither :: forall e a. Covered e a -> Either e a
toEither (Recovered err _) = Left err
toEither (Carried x) = Right x


toMaybe :: forall e a. Covered e a -> Maybe a
toMaybe (Recovered _ _) = Nothing
toMaybe (Carried x) = Just x


recover :: forall e a. Covered e a -> a
recover (Recovered _ x) = x
recover (Carried x) = x


uncover :: forall e a. Covered e a -> Maybe e /\ a
uncover covered = hasError covered /\ recover covered


uncover' :: forall m e a. Monoid (m e) => Covered (m e) a -> m e /\ a
uncover' covered = (hasError covered # fromMaybe mempty) /\ recover covered


mapError :: forall ea eb a. (ea -> eb) -> Covered ea a -> Covered eb a
mapError f (Recovered err v) = Recovered (f err) v
mapError _ (Carried v) = Carried v


hasError :: forall e a. Covered e a -> Maybe e
hasError (Recovered err _) = Just err
hasError (Carried _) = Nothing


withError :: forall a e x. (e -> a -> a) -> a -> Covered e x -> a
withError f a (Recovered err _) = f err a
withError _ a _ = a


run :: forall a e x. Semigroup a => (e -> a) -> (x -> a) -> Covered e x -> a
run errF subjF (Recovered err x) = errF err <> subjF x
run errF subjF (Carried x) = subjF x


-- FIXME: change to Monoid
appendError :: forall m e a. Monoid (m e) => Applicative m => Covered e a -> Covered (m e) a -> Covered (m e) a
appendError (Recovered err x) (Recovered errors _) = Recovered (errors <> pure err <> mempty) x
appendError (Recovered err x) (Carried _) = Recovered (pure err) x
appendError (Carried x) (Carried _) = Recovered mempty x
appendError (Carried x) (Recovered errors _) = Recovered errors x
