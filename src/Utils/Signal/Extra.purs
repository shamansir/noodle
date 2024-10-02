module Signal.Extra where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Tuple.Nested (type (/\), (/\))
-- import Data.Semigroup.Foldable (class Foldable1)
import Data.Maybe (fromMaybe)
import Data.Foldable (class Foldable)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty (head) as NE

import Signal (Signal)
import Signal (runSignal, foldp, get, merge, mergeMany) as Signal
-- import Data.Monoid (mempty)


infixl 5 Signal.merge as >*<


class RunInSignal :: forall k. (k -> Type) -> Constraint
class RunInSignal m where
    runInSignal :: forall a. m a -> Effect Unit


instance RunInSignal Effect where
    runInSignal ma = ma *> pure unit


instance RunInSignal Aff where
    runInSignal ma = runAff_ (const $ pure unit) ma *> pure unit


runSignal :: forall m. RunInSignal m => MonadEffect m => Signal (m Unit) -> m Unit
runSignal sig = liftEffect $ Signal.runSignal (runInSignal <$> sig)


indexed :: forall a. a -> Signal a -> Signal (Int /\ a)
indexed default =
    Signal.foldp (\a (prevIdx /\ _) -> prevIdx /\ a) (0 /\ default) -- Signal.sampleOn ?wh (0 /\ default)


indexedFromCurrent :: forall a m. MonadEffect m => Signal a -> m (Signal (Int /\ a))
indexedFromCurrent sig = do
    cur <- liftEffect $ Signal.get sig
    pure $ Signal.foldp (\a (prevIdx /\ _) -> prevIdx /\ a) (0 /\ cur) sig


mergeManyNE :: forall f a. Functor f => Foldable f => NonEmpty f (Signal a) -> Signal a
mergeManyNE fs = Signal.mergeMany fs # fromMaybe (NE.head fs)