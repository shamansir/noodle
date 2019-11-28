module Rpd.API.Covered where


import Prelude

import Data.Maybe
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))

{- inspired by http://hackage.haskell.org/package/hexpr-0.0.0.0/docs/Control-Monad-Errors.html -}


-- newtype Covered errors state =
--     CoveredT (Monad m => Monoid errors => m (errors -> (Maybe state /\ errors)))


newtype Covered errors m state =
    Covered (m (Maybe errors -> (Maybe state /\ Maybe errors)))


nothing :: forall errors m state. Monad m => Covered errors m state
nothing =
    Covered $ pure $ const (Nothing /\ Nothing)


cover :: forall m error state. Monad m => error -> Covered error m state
cover error = nothing


cover' :: forall m errors state error. Monad m => Semigroup errors => error -> Covered errors m state
cover' errors = nothing


coverAll :: forall m errors state. Monad m => Semigroup errors => errors -> Covered errors m state
coverAll = cover


recover :: forall errors m state. Monad m => Covered errors m state -> m (Maybe state /\ Maybe errors)
recover (Covered action) = do
    innerAction <- action
    pure $ innerAction Nothing


-- coverEither :: forall m errors state error. Monad m => Semigroup errors => Either error state -> Covered errors m state
-- coverEither (Left error) =
--     nothing
