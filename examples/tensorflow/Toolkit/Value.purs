module TensorFlow.Toolkit.Value where


import Prelude

import Data.Array (length) as Array
import Data.Maybe

import TensorFlow.TfModel


data Value
    = Bang
    | TF TfModel
    | Code String


instance showValue :: Show Value where
    show Bang = "◌"
    show (TF _) = "TF"
    show (Code str) = str
