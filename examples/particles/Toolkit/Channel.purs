module Example.Toolkit.Channel where

import Prelude (class Show, identity)

import Rpd.Toolkit as T

import Example.Toolkit.Value


data Channel
    = ColorChannel
    | ShapeChannel
    | NumericChannel
    | TimeChannel
    | TriggerChannel


instance showChannel :: Show Channel where
    show ColorChannel = "color"
    show ShapeChannel = "shape"
    show NumericChannel = "number"
    show TimeChannel = "time"
    show TriggerChannel = "trigger"


instance exampleChannel :: T.Channels Value Channel where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity
