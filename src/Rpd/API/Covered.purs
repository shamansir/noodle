module Rpd.API.Covered where


import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple.Nested ((/\), type (/\))

import Rpd.API.Errors


data Covered a
    = Recovered RpdError a
    | Carried a


instance coveredFunctor :: Functor Covered where
    map f (Recovered err a) = Recovered err $ f a
    map f (Carried a) = Carried $ f a


instance coveredApply :: Apply Covered where
    apply (Recovered _ f) covered = f <$> covered
    apply (Carried f) covered = f <$> covered


instance coveredApplicative :: Applicative Covered where
    pure = Carried


instance coveredBind :: Bind Covered where
    bind covered k = k $ recover covered


instance coveredMonad :: Monad Covered


carry :: forall a. a -> Covered a
carry = Carried


cover :: forall a. a -> RpdError -> Covered a
cover x err = Recovered err x


fromEither :: forall a. Either RpdError a -> a -> Covered a
fromEither (Right v) _ = Carried v
fromEither (Left err) fallback = Recovered err fallback


recover :: forall a. Covered a -> a
recover (Recovered _ x) = x
recover (Carried x) = x


uncover :: forall a. Covered a -> Maybe RpdError /\ a
uncover covered = getError covered /\ recover covered


getError :: forall a. Covered a -> Maybe RpdError
getError (Recovered err _) = Just err
getError (Carried _) = Nothing


