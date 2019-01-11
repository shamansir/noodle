module Example.Network
    ( network
    ) where

import Prelude

import Rpd.Network (empty) as Network
import Rpd.Network as R
import Rpd.API as R

import Example.Toolkit

network :: forall d. R.Rpd (R.Network d)
network = pure $ Network.empty "foo"
