module Example.Network
    ( network
    ) where

import Prelude

import Rpd.Network (empty) as Network
import Rpd.Network as R
import Rpd.Path as R
import Rpd.API (Rpd) as R
import Rpd.API as Rpd
import Rpd.API ((</>))

import Example.Toolkit (testPatch, testNode)

network :: forall d. R.Rpd (R.Network d)
network =
    Rpd.init "foo"
        </> Rpd.addPatch' testPatch
        </> Rpd.addNode' (R.patchId 0) testNode
