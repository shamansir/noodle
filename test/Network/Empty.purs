module RpdTest.Network.Empty
    ( network, MyData ) where

import Rpd (empty, Network) as R

data MyData
  = Bang

network :: R.Network MyData
network =
  R.empty
