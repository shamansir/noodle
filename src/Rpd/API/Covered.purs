module Rpd.API.Covered where


import Prelude

import Data.Maybe
import Data.Either
import Data.Array (snoc)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (class Bifunctor)

{- inspired by http://hackage.haskell.org/package/hexpr-0.0.0.0/docs/Control-Monad-Errors.html -}


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
