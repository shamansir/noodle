module HydraTk.Synth where

import Prelude

import Effect (Effect)
import Noodle.Id (NodeR) as Id

import Data.Maybe (Maybe(..))

import Front.Shared.Bounds (Bounds)


foreign import runHydra :: Effect Unit
foreign import resize :: Int -> Int -> Effect Unit
foreign import executeHydra :: String -> Effect Unit
foreign import drawSceneAt :: Id.NodeR -> { node :: Bounds, body :: Bounds } -> Unit -> Effect Unit