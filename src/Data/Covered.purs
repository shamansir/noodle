module Data.Covered where


import Prelude

import Control.Alt (class Alt)
import Data.Either
import Data.Maybe
import Data.Maybe (fromMaybe) as Maybe
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))


data Covered e a
    = Recovered e a
    | Carried a


-- instance coveredSemigroup :: Semigroup (Covered e) where
--     append (Covered err a)


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


instance coveredBind :: Semigroup e => Bind (Covered e) where
    -- bind covered k = k $ recover covered
    -- Above breaks the second law: Recovered err x >>= pure != Recovered err x, but Carried x
    bind covered k = appendErrors covered $ k $ recover covered


instance coveredAlt :: Alt (Covered e) where
    alt = consider


instance coveredSemigroup :: (Semigroup e, Semigroup a) => Semigroup (Covered e a) where
    append (Recovered errorsA valA) (Recovered errorsB valB) =
        Recovered (errorsA <> errorsB) (valA <> valB)
    append (Recovered errorsA valA) (Carried valB) =
        Recovered errorsA (valA <> valB)
    append (Carried valA) (Recovered errorsB valB) =
        Recovered errorsB (valA <> valB)
    append (Carried valA) (Carried valB) =
        Carried (valA <> valB)

-- TODO: Semigroup, + like Tuple has, see Tuple Apply, but that would join data, not errors


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


recover :: forall e a. Covered e a -> a
recover (Recovered _ x) = x
recover (Carried x) = x


uncover :: forall e a. Covered e a -> Maybe e /\ a
uncover covered = hasError covered /\ recover covered


uncover' :: forall m e a. Monoid (m e) => Covered (m e) a -> m e /\ a
uncover' covered = (hasError covered # Maybe.fromMaybe mempty) /\ recover covered


consider :: forall e a. Covered e a -> Covered e a -> Covered e a
consider (Recovered errA _) (Carried vB) = Recovered errA vB
consider _ coveredB = coveredB


inject :: forall e a b. b -> Covered e a -> Covered e b
inject b covered = const b <$> covered


fromMaybe :: forall e a. e -> a -> Maybe a -> Covered e a
fromMaybe _ _ (Just v)  = Carried v
fromMaybe e v Nothing  = Recovered e v


toMaybe :: forall e a. Covered e a -> Maybe a
toMaybe (Recovered _ _) = Nothing
toMaybe (Carried x) = Just x


fromEither :: forall e a. a -> Either e a -> Covered e a
fromEither v either = fromEither' (const v) either


fromEither' :: forall e a. (Unit -> a) -> Either e a -> Covered e a
fromEither' _ (Right v) = Carried v
fromEither' fallback (Left err)  = Recovered err $ fallback unit


toEither :: forall e a. Covered e a -> Either e a
toEither (Recovered err _) = Left err
toEither (Carried x) = Right x


appendErrors :: forall e a b. Semigroup e => Covered e a -> Covered e b -> Covered e b
appendErrors (Recovered errorsA _) (Recovered errorsB val) = Recovered (errorsA <> errorsB) val
appendErrors (Recovered errors _) (Carried val) = Recovered errors val
appendErrors (Carried _) (Carried val) = Carried val
appendErrors (Carried _) (Recovered errors val) = Recovered errors val


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


stack
    :: forall m e a
     . Monoid (m e)
    => Applicative m
    => Covered e a
    -> Covered (m e) a
    -> Covered (m e) a
stack (Recovered err val) (Recovered errors _) = Recovered (errors <> pure err) val
stack (Recovered err val) (Carried _) = Recovered (pure err) val
stack (Carried val) (Carried _) = Recovered mempty val
stack (Carried val) (Recovered errors _) = Recovered errors val


instance showCovered :: (Show e, Show a) => Show (Covered e a) where
    show (Recovered e a) = "Recovered " <> show e <> " " <> show a
    show (Carried a) = "Carried " <> show a


instance eqCovered :: (Eq e, Eq a) => Eq (Covered e a) where
    eq (Recovered errA valA) (Recovered errB valB) = errA == errB && valA == valB
    eq (Carried valA) (Carried valB) = valA == valB
    eq _ _ = false
