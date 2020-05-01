module Xodus.Toolkit.Channel where


import Prelude (class Show, identity)

import Noodle.Toolkit as T

import Xodus.Toolkit.Value


data Channel = Channel


instance showChannel :: Show Channel where
    show Channel = "Channel"


instance xodusChannel :: T.Channels Value Channel where
    default _ = Value_
    accept _ _ = true
    adapt _ = identity
