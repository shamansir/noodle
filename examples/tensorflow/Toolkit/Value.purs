module TensorFlow.Toolkit.Value where


import Prelude

import Data.Array (length) as Array
import Data.Maybe

import TensorFlow.TfModel
import Data.Tuple.Nested ((/\))


data Value
    = Shape Shape
    | TF TfModel
    | Code String


instance showValue :: Show Value where
    show (Shape (n1 /\ n2 /\ n3)) = show n1 <> ":" <> show n2 <> ":" <> show n3
    show (TF _) = "TF"
    show (Code str) = str
