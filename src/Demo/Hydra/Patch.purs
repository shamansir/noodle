module HydraTk.Patch where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Data.Maybe (Maybe(..))

import Halogen (RefLabel)

import HydraTk.Synth (runHydra) as HydraSynth


newtype PState =
    PState
        { canvasRef :: Maybe RefLabel }


init :: forall m. MonadEffect m => m PState
init =
    liftEffect HydraSynth.runHydra *> pure (PState { canvasRef : Nothing })
    -- liftEffect runHydra >>= (const $ pure (PState { canvasRef : Nothing }))