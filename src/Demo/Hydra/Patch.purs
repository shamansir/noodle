module HydraTk.Patch where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen (RefLabel)


newtype PState =
    PState
        { canvasRef :: Maybe RefLabel }


init :: PState
init =
    PState
        { canvasRef : Nothing }