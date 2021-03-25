module RayDraw.Toolkit.Channel where


import Prelude (class Show, identity)

import Noodle.Toolkit as Toolkit

import RayDraw.Toolkit.Value


data Channel = Channel


instance showChannel :: Show Channel where
    show Channel = "Channel"


instance raydrawChannel :: Toolkit.Channels Value Channel where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity
