module TensorFlow.Toolkit.Channel where


import Prelude (class Show, identity)

import Noodle.Toolkit as T

import TensorFlow.Toolkit.Value
import TensorFlow.TfModel

data Channel = Channel


instance showChannel :: Show Channel where
    show Channel = "Channel"


instance tensorFlowChannel :: T.Channels Value Channel where
    default _ = TF Empty
    accept _ _ = true
    adapt _ = identity
