module Signal.Extra where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)

import Data.Tuple.Nested (type (/\), (/\))

import Signal (Signal)
import Signal (runSignal, foldp, get) as Signal


runSignal :: forall m. MonadEffect m => Signal (m Unit) -> m Unit
runSignal = unsafeCoerce $ Signal.runSignal


indexed :: forall a. a -> Signal a -> Signal (Int /\ a)
indexed default =
    Signal.foldp (\a (prevIdx /\ _) -> prevIdx /\ a) (0 /\ default) -- Signal.sampleOn ?wh (0 /\ default)


indexedFromCurrent :: forall a m. MonadEffect m => Signal a -> m (Signal (Int /\ a))
indexedFromCurrent sig = do
    cur <- liftEffect $ Signal.get sig
    pure $ Signal.foldp (\a (prevIdx /\ _) -> prevIdx /\ a) (0 /\ cur) sig