module Rpd.Toolkit
    (Toolkit) where

import Rpd.Util (type (/->))
import Rpd.Def as D

type Toolkit d =
    { id :: String
    , nodes :: String /-> D.NodeDef d
    , channels :: String /-> D.ChannelDef d
    }
