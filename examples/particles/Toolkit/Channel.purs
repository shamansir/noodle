module Example.Toolkit.Channel where

import Prelude (class Show, identity)

import Rpd.Toolkit as T

import Example.Toolkit.Value


data Channel
    = TriggerChannel
    | NumericalChannel
    | AnimationChannel -- TODO: accept both `Spread` and `Apply`


instance showChannel :: Show Channel where
    show _ = "c"


instance exampleChannel :: T.Channels Value Channel where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity
