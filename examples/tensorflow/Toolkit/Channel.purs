module TensorFlow.Toolkit.Channel where


import Prelude (class Show, identity)

import Noodle.Toolkit as T

import TensorFlow.Toolkit.Value


data Channel = Channel


instance showChannel :: Show Channel where
    show Channel = "Channel"


instance tensorFlowChannel :: T.Channels Value Channel where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity
