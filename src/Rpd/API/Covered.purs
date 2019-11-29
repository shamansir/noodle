module Rpd.API.Covered where


import Prelude

import Data.Maybe
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (class Bifunctor)

{- inspired by http://hackage.haskell.org/package/hexpr-0.0.0.0/docs/Control-Monad-Errors.html -}


data Covered errors state =
    Covered (Maybe errors) (Maybe state)


nothing :: forall errors state. Covered errors state
nothing =
    Covered Nothing Nothing


uncover :: forall errors state. Covered errors state -> Maybe errors /\ Maybe state
uncover (Covered maybeErrors maybeState) = maybeErrors /\ maybeState


notice :: forall error state. error -> Covered error state
notice error = Covered (Just error) Nothing


cover :: forall error state. state -> Covered error state
cover state = Covered Nothing $ Just state


-- covered :: forall error state. state -> Covered error state


hoist
    :: forall errors state
     . Semigroup errors
    => errors
    -> Covered errors state
    -> Covered errors state
hoist errors (Covered (Just prevErrors) maybeState) =
    Covered (Just $ errors <> prevErrors) maybeState
hoist errors (Covered Nothing maybeState) =
    Covered (Just errors) maybeState


hoistOne :: forall error state. error -> Covered error state -> Covered error state
hoistOne error (Covered _ maybeState) = Covered (Just error) maybeState


coverIn :: forall error state. state -> Covered error state -> Covered error state
coverIn state (Covered maybeErrors _) = Covered maybeErrors $ Just state


instance functorCovered :: Functor (Covered errors) where
    map f (Covered maybeErrors maybeState) = Covered maybeErrors $ f <$> maybeState


instance bifunctorCovered :: Bifunctor Covered where
  bimap f g (Covered maybeErrors maybeState) = Covered (f <$> maybeErrors) (g <$> maybeState)


instance applyCovered :: Apply (Covered errors) where
  apply (Covered maybeErrors maybeF) (Covered maybeOtherErrors maybeState) =
    Covered maybeErrors (maybeF <*> maybeState) -- FIXME: wrong, does not satisfy the law


-- instance applicativeCovered :: Applicative (Covered errors) where
--   pure = cover


instance bindEither :: Bind (Covered errors) where
  bind (Covered maybeErrors Nothing) _ = Covered maybeErrors Nothing
  bind (Covered maybeErrors (Just state)) f = f state


fromEither :: forall error state. Either error state -> Covered error state
fromEither = either notice cover


fromMaybe :: forall error state. Maybe state -> Covered error state
fromMaybe = maybe nothing cover

-- coverEither :: forall m errors state error. Monad m => Semigroup errors => Either error state -> Covered errors m state
-- coverEither (Left error) =
--     nothing
