module TensorFlow.Toolkit.Value where


import Prelude

import Data.Array (length) as Array
import Data.Maybe


data Value
    = Bang
    | Layer Int


instance showValue :: Show Value where
    show Bang = "◌"
    show (Layer n) = "layer " <> show n
