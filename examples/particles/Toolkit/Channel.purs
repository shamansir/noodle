module Example.Toolkit.Channel where

import Prelude (class Show, identity)

import Rpd.Toolkit as T

import Example.Toolkit.Value


data Channel
    = TriggerChannel -- OnlyTriggers
    | NumericalChannel -- OnlyNumbers
    | ColorChannel -- OnlyColors
    | VectorChannel -- OnlyVectors
    | SpreadChannel -- OnlySpreads
    | AnyValueChannel --


instance showChannel :: Show Channel where
    show _ = "c"


instance exampleChannel :: T.Channels Value Channel where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity
