module Noodle.Node.WithChannels
    where


import Noodle.Node as N
import Noodle.Node.Channel as N

import Data.Tuple.Nested (type (/\), (/\))


type Node d =
    N.Node'
        d
        (String /\ d /\ N.Channel d) /\ (String /\ d /\ N.Channel d)
