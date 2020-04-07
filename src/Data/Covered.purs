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


-- see Tuple Apply

instance coveredApply :: Apply (Covered e) where
    apply (Recovered _ f) covered = f <$> covered
    apply (Carried f) covered = f <$> covered


instance coveredApplicative :: Applicative (Covered e) where
    pure = Carried


instance coveredBind :: Bind (Covered e) where
    bind covered k = k $ recover covered


instance coveredMonad :: Monad (Covered e)


-- TODO: Comonad


run :: forall a e x. Semigroup a => (e -> a) -> (x -> a) -> Covered e x -> a
run errF subjF (Recovered err v) = errF err <> subjF v
run errF subjF (Carried v) = subjF v


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


fromEither' :: forall e a. (Unit -> a) -> Either e a -> Covered e a
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


containsError :: forall e a. Eq e => e -> Covered e a -> Boolean
containsError err (Recovered errInside _) = err == errInside
containsError _ _ = false


whenC :: forall e a x. (a -> x) -> Covered e a -> Covered e a /\ x
whenC f (Recovered err v) = Recovered err v /\ f v
whenC f (Carried v) = Carried v /\ f v


-- traverse?
unpack :: forall e a x. Covered e (a /\ x) -> Covered e a /\ x
unpack (Recovered err (v /\ x)) = Recovered err v /\ x
unpack (Carried (v /\ x)) = Carried v /\ x


appendError
    :: forall m e a
     . Monoid (m e)
    => Applicative m
    => Covered e a
    -> Covered (m e) a
    -> Covered (m e) a
appendError (Recovered err val) (Recovered errors _) = Recovered (errors <> pure err <> mempty) val
appendError (Recovered err val) (Carried _) = Recovered (pure err) val
appendError (Carried val) (Carried _) = Recovered mempty val
appendError (Carried val) (Recovered errors _) = Recovered errors val


joinErrors :: forall e a. Semigroup e => Covered e a -> Covered e a -> Covered e a
joinErrors (Recovered errorsA _) (Recovered errorsB val) = Recovered (errorsA <> errorsB) val
joinErrors (Recovered errors _) (Carried val) = Recovered errors val
joinErrors (Carried _) (Carried val) = Carried val
joinErrors (Carried _) (Recovered errors val) = Recovered errors val


instance showCovered :: (Show e, Show a) => Show (Covered e a) where
    show (Recovered e a) = "Recovered " <> show e <> " " <> show a
    show (Carried a) = "Carried " <> show a


instance eqCovered :: (Eq e, Eq a) => Eq (Covered e a) where
    eq (Recovered errA valA) (Recovered errB valB) = errA == errB && valA == valB
    eq (Carried valA) (Carried valB) = valA == valB
    eq _ _ = false
