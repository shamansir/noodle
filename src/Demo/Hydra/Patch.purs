module HydraTk.Patch where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Maybe (Maybe(..))

import Halogen (RefLabel)


newtype PState =
    PState
        { canvasRef :: Maybe RefLabel }


init :: forall m. MonadEffect m => m PState
init =
    liftEffect runHydra *> pure (PState { canvasRef : Nothing })
    -- liftEffect runHydra >>= (const $ pure (PState { canvasRef : Nothing }))


foreign import runHydra :: Effect Unit
foreign import resize :: Int -> Int -> Effect Unit
foreign import executeHydra :: String -> Effect Unit