module Rpd.API.Covered where


import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple.Nested ((/\), type (/\))


data Covered e a
    = Recovered e a
    | Carried a


instance coveredFunctor :: Functor (Covered e) where
    map f (Recovered err a) = Recovered err $ f a
    map f (Carried a) = Carried $ f a


instance coveredApply :: Apply (Covered e) where
    apply (Recovered _ f) covered = f <$> covered
    apply (Carried f) covered = f <$> covered


instance coveredApplicative :: Applicative (Covered e) where
    pure = Carried


instance coveredBind :: Bind (Covered e) where
    bind covered k = k $ recover covered


instance coveredMonad :: Monad (Covered e)


carry :: forall e a. a -> Covered e a
carry = Carried


cover :: forall e a. a -> e -> Covered e a
cover x err = Recovered err x


fromEither :: forall e a. Either e a -> a -> Covered e a
fromEither (Right v) _ = Carried v
fromEither (Left err) fallback = Recovered err fallback


recover :: forall e a. Covered e a -> a
recover (Recovered _ x) = x
recover (Carried x) = x


uncover :: forall e a. Covered e a -> Maybe e /\ a
uncover covered = getError covered /\ recover covered


getError :: forall e a. Covered e a -> Maybe e
getError (Recovered err _) = Just err
getError (Carried _) = Nothing


